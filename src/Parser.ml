open Program

let rec parse_atom tok =
    match tok with
        [] -> []
        | Token.Int x :: t -> Expression (Constant (VInt x)) :: parse_atom t
        | Token.Float (x, y) :: t -> Expression (Constant (VFloat x)) :: parse_atom t
        | Token.String x :: t -> Expression (Constant (VString x)) :: parse_atom t
        | Token.Char x :: t -> Expression (Constant (VChar x)) :: parse_atom t
        | Token.Id x :: t -> Expression (Variable (TextID x)) :: parse_atom t
        | h :: t -> Raw h :: parse_atom t

let rec fold_expr bg en =
    let open DList in
    if not (equal bg en) && not (equal (get bg.next) (get en.prev)) then
    begin
        (* fold after keywords else, in, ->, , *)
        let fold_after = [Token.Keyword "else"; Token.Keyword "in"; Token.Arrow; Token.Comma] in
        let it = ref (get bg.next) in
        while not (equal !it en) do
            begin match !it.item with
                Some (Raw token) when List.mem token fold_after ->
                    ignore (fold_expr !it en)
                | _ -> ()
            end;
            it := get !it.next
        done;

        (* brackets *)
        let it = ref (get (get bg.next).next) in
        while not (equal !it en) && not (equal !it (get en.prev)) do
            begin match !it.item, (get !it.prev).item, (get !it.next).item with
                Some (Expression e), Some (Raw (Token.LeftBracket)), Some (Raw (Token.RightBracket)) ->
                    ignore (remove (get !it.next));
                    ignore (remove (get !it.prev))
                | Some (RawTuple t), Some (Raw (Token.LeftBracket)), Some (Raw (Token.RightBracket)) ->
                    ignore (remove (get !it.next));
                    ignore (remove (get !it.prev));
                    !it.item <- Some (Expression (Tuple t))
                | _ -> ()
            end;
            it := get !it.next
        done;

        (*if then else, let =, and =, fun -> *)
        let module Compound = struct type ctype =
                { func: statement list -> statement; argc: int }
        end in let open Compound in
        let comps = [("if", { func = (fun [Expression cond;
                                                Raw (Token.Keyword "then");
                                                Expression a;
                                                Raw (Token.Keyword "else");
                                                Expression b] ->
                                                Expression (IfElse (cond, a, b))); argc = 5 });
                        ("fun", { func = (fun [Expression pat;
                                                Raw Token.Arrow;
                                                Expression e] ->
                                                Expression (Lambda (pat, e)));
                                                argc = 3});
                        ("let", { func = (fun [Expression pat;
                                                Raw Token.Equality;
                                                Expression e] ->
                                                DefinitionList [(pat, e)]); argc = 3});
                        ("and", { func = (fun [Expression pat;
                                                Raw Token.Equality;
                                                Expression e] ->
                                                Definition (pat, e)); argc = 3})] in
        let it = ref (get bg.next) in
        while not (equal !it en) do
            begin match !it.item with
                Some (Raw (Token.Keyword key)) ->
                    begin try let comp = List.assoc key comps in
                        let rec cut_n it n =
                            if n = 0 then []
                            else if equal it en then failwith "Parse error: unexpected end of expression"
                            else let ret = get it.item :: cut_n (get it.next) (n-1) in
                                    remove it; ret
                        in let l = cut_n (get !it.next) comp.argc
                        in !it.item <- Some (comp.func l)
                    with Not_found -> () | _ -> failwith "Parse error: syntax error"
                    end
                | _ -> ()
            end;
            it := get !it.next
        done;

        let it = ref (get (get bg.next).next) in
        while not (equal !it en) do
            begin match !it.item, (get !it.prev).item with
                Some (Definition d), Some (DefinitionList l) ->
                    !it.item <- Some (DefinitionList (d :: l));
                    ignore (remove (get !it.prev))
                | Some (Raw (Token.Keyword "in")), Some (DefinitionList d) ->
                begin
                    let next = get !it.next in
                    if not (equal next en) then
                        match next.item with
                            Some (Expression e) ->
                                !it.item <- Some (Expression (LetIn (d, e)));
                                ignore (remove next);
                                ignore (remove (get !it.prev))
                            | _ -> ()
                end
                | _ -> ()
            end;
            it := get !it.next
        done;

        let it = ref (get (get bg.next).next) in
        while not (equal !it en) && not (equal !it (get en.prev)) do
            begin match !it.item, (get !it.prev).item, (get !it.next).item with
                Some (Expression e), Some (Raw (Token.LeftBracket)), Some (Raw (Token.RightBracket)) ->
                    ignore (remove (get !it.next));
                    ignore (remove (get !it.prev))
                | _ -> ()
            end;
            it := get !it.next
        done;

        (* unary operators*)
        let it = ref (get (get en.prev).prev) in 
        while not (equal !it bg) do
            begin match !it.item, (get !it.next).item with
                    Some (Raw (Token.UnaryOperator op)), Some (Expression e) ->
                        !it.item <- Some (Expression (UnaryOperator (TextID op, e)));
                        ignore (remove (get !it.next))
                | _ -> ()
            end;
            it := get !it.prev
        done;

        (* functions *)
        let it = ref (get (get bg.next).next) in
        while not (equal !it en) do
            begin match (get !it.prev).item, !it.item with
                Some (Expression a), Some (Expression b) ->
                    !it.item <- Some (Expression (Call (a, b)));
                    ignore (remove (get !it.prev))
                | _ -> ()
            end;
            it := get !it.next
        done;

        (* binary operators *)
        let fold_bin filter =
            let it = ref (get (get bg.next).next) in
            while not (equal !it en) && not (equal !it (get en.prev)) do
                begin match !it.item, (get !it.prev).item, (get !it.next).item with
                    Some (Raw (Token.BinaryOperator op)), Some (Expression a), Some (Expression b)
                        when filter op ->
                            !it.item <- Some (Expression (BinaryOperator (TextID op, a, b)));
                            ignore (remove (get !it.next));
                            ignore (remove (get !it.prev))
                    | _ -> ()
                end;
                it := get !it.next
            done
            in
        let list_filter l x = List.mem x l in
        fold_bin (list_filter ["**"]);
        fold_bin (list_filter ["*"; "/"; "%"]);
        fold_bin (list_filter ["+"; "-"]);
        fold_bin (list_filter ["^";"|";"&"]);
        fold_bin (list_filter ["==";">";"<";">=";"<="]);
        fold_bin (list_filter ["||";"&&"]);
        fold_bin (list_filter ["::"]);
        fold_bin (fun _ -> true);

        (*commas*)
        let it = ref (get (get bg.next).next) in
        while not (equal !it en) && not (equal !it (get en.prev)) do
            begin match !it.item, (get !it.prev).item, (get !it.next).item with
                Some (Raw (Token.Comma)), Some (Expression a), Some (Expression b) ->
                    ignore (remove (get !it.next));
                    ignore (remove (get !it.prev));
                    !it.item <- Some (RawTuple ([a; b]))
                | Some (Raw (Token.Comma)), Some (Expression a), Some (RawTuple b)->
                    ignore (remove (get !it.next));
                    ignore (remove (get !it.prev));
                    !it.item <- Some (RawTuple (a::b))
                | _ -> ()
            end;
            it := get !it.next
        done(*;

        if not (equal (get bg.next) (get en.prev)) then failwith "Parse error: failed to fold all tokens"*)
    end

let left_brackets = [Token.LeftBracket; Token.Keyword "if"; Token.Keyword "let"; Token.Keyword "fun"]
let separators = [Token.Keyword "then"; Token.Keyword "and"; Token.Equality]
let right_brackets = [Token.RightBracket; Token.Keyword "else"; Token.Keyword "in"; Token.Arrow]
let fold_brackets dl =
    let open DList in
    let it = ref (get dl.next) in
    let st = Stack.create () in
    while !it.item <> None do
        begin match !it.item with
            Some (Raw x) ->
                if List.mem x separators || List.mem x right_brackets then begin
                    let prev = try Stack.pop st with Stack.Empty -> failwith "Parse error: unmatched token" in 
                    ignore (fold_expr prev !it)
                end;
                if List.mem x separators || List.mem x left_brackets then
                    Stack.push !it st
            | _ -> ()
        end;
        it := get !it.next
    done;
    while not (Stack.is_empty st) do
        ignore (fold_expr (Stack.pop st) !it)
    done

let rec fix_fun statement =
    let rec go pat f =
        match pat with
            Call(p, e) -> go p (Lambda (e, f))
            | _ -> (pat, f)
        in
    let rec go_expr e =
        match e with
            Call (a, b) -> Call (go_expr a, go_expr b)
            | Lambda (a, b) -> Lambda (a, go_expr b)
            | Tuple t -> Tuple (List.map go_expr t)
            | UnaryOperator (id, e) -> UnaryOperator (id, go_expr e)
            | BinaryOperator (id, a, b) -> BinaryOperator (id, go_expr a, go_expr b)
            | IfElse (a, b, c) -> IfElse (go_expr a, go_expr b, go_expr c)
            | LetIn (def, e) ->
                let DefinitionList def = fix_fun (DefinitionList def) in
                LetIn (def, go_expr e)
            | _ -> e
        in
    match statement with
        DefinitionList [] -> DefinitionList []
        | DefinitionList ((p, e)::t) -> let DefinitionList t = fix_fun (DefinitionList t) in
                                        DefinitionList (go p e :: t)
        | Expression e -> Expression (go_expr e)
        | _ -> statement

let parse tok =
    if tok = [] then failwith "Expression is empty" else
    let open DList in
    let dl = of_list (parse_atom tok) in
    fold_brackets dl;
    fold_expr dl (_end dl);
    fix_fun (get (get dl.next).item)