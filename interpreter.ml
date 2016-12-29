let string_of_char c = String.make 1 c

module type VectorType =
sig
    type 'a t

    val create: 'a -> 'a t
    val length: 'a t -> int
    val append: 'a t -> 'a -> unit
    val get: 'a t -> int -> 'a
    val set: 'a t -> int -> 'a -> unit
end

module Vector : VectorType =
struct
    type 'a t = { mutable arr: 'a array; mutable len: int; default: 'a }

    let create default = { arr=Array.make 1 default; len=0; default=default }

    let reserve vec n =
        let new_arr = Array.make n vec.default in
        Array.blit vec.arr 0 new_arr 0 vec.len;
        vec.arr <- new_arr

    let length vec = vec.len

    let append vec a =
        if vec.len = Array.length vec.arr then reserve vec (vec.len * 2);
        Array.set vec.arr vec.len a;
        vec.len <- vec.len + 1

    let get vec n = 
        if n >= vec.len 
        then raise (Invalid_argument "index out of bounds")
        else Array.get vec.arr n
    let set vec n a =
        if n >= vec.len
        then raise (Invalid_argument "index out of bounds")
        else Array.set vec.arr n a

end

module DList =
struct
    type 'a t = { mutable prev: 'a t option; mutable next: 'a t option; mutable item: 'a option; _id: int }
    let cur_id = ref 0

    let equal a b =
        a._id = b._id

    let get (Some x) = x

    let create () =
        let rec a = { prev = None; next = Some b; item = None; _id = !cur_id }
            and b = { prev = Some a; next = None; item = None; _id = !cur_id + 1 }
        in cur_id := !cur_id + 2; a
    let remove x =
        (get x.prev).next <- x.next;
        (get x.next).prev <- x.prev;
        get x.next

    let insert_after a l =
        let it = { prev = Some l; next = l.next; item = Some a; _id = !cur_id } in
        cur_id := !cur_id + 1;
        (get l.next).prev <- Some it;
        l.next <- Some it;
        l

    let rec of_list l =
        match l with
            [] -> create ()
          | h::t -> insert_after h (of_list t)

    let rec _begin l =
        match l.prev with
            None -> l
          | Some p -> _begin p
    
    let rec _end l =
        match l.next with
            None -> l
          | Some n -> _end n

    let rec to_list a b =
        if equal a b || a.next = None then []
        else let nx = get a.next in
            if equal nx b then []
            else get nx.item :: to_list nx b

end

module type TokenType =
sig
    type t = LeftBracket 
            | RightBracket
            | Comma
            | SemiColon
            | Equality
            | Arrow
            | Keyword of string
            | Operator of string
            | UnaryOperator of string
            | BinaryOperator of string
            | Int of int
            | Float of float * float
            | String of string
            | Char of char
            | Id of string
            | Raw of char list

    val get_tokens: string -> t list
end

module Token : TokenType =
struct
    type t = LeftBracket 
            | RightBracket
            | Comma
            | SemiColon
            | Equality
            | Arrow
            | Keyword of string
            | Operator of string
            | UnaryOperator of string
            | BinaryOperator of string
            | Int of int
            | Float of float * float
            | String of string
            | Char of char
            | Id of string
            | Raw of char list

    type character = Digit | Letter | Whitespace | Dot | Special1 | Special2

    let characters = [(Digit, "0123456789");
                    (Letter, "qwertyuiopasdfghjklzxcvbnmQWERTYUIOPASDFGHJKLZXCVBNM_");
                    (Whitespace, " \n\t\r");
                    (Dot, ".");
                    (Special1, "();,");
                    (Special2, "@$:~!%^&*-+=[]|<,>/")]

    let keywords =
        let rec make_hashtbl tbl l =
            match l with [] -> tbl | h::t -> Hashtbl.add tbl h h; make_hashtbl tbl t
        and keywords_list = ["let"; "in"; "and"; "if"; "then"; "else"; "fun"]
        in  make_hashtbl (Hashtbl.create 10) keywords_list

    let special1_type c = let specials = [('(', LeftBracket);
                                          (')', RightBracket); 
                                          (';', SemiColon);
                                          (',', Comma)] in
        List.assoc c specials

    let char_type c =
        try (let (result, _) =
            List.find (function (_, str) -> String.contains str c) characters in result)
        with Not_found -> failwith ("Invalid_character: " ^ (string_of_char c))

    let parse_text tokens =
        let special = [('n', '\n');('t', '\t');('\\', '\\')] in
        let special_string, special_char = ('"', '"')::special, ('\'', '\'')::special in
        let get_string str =
            let buf = Buffer.create 1 in
            let rec go str =
                match str with
                    '\\'::c::t -> 
                        (try Buffer.add_char buf (List.assoc c special_string); go t
                        with Not_found -> failwith "Syntax error: wrong letter after backslash")
                | '"'::t -> t
                | c::t -> Buffer.add_char buf c; go t
                | _ -> failwith "Syntax error"
                in
            let tail = go str in (Buffer.contents buf, tail) in
        let get_char str =
            match str with
                '\\'::c::'\''::t -> (try (List.assoc c special_char, t)
                    with Not_found -> failwith "Syntax error: wrong letter after backslash")
                | c::'\''::t when c <> '\\' -> (c, t)
                | _ -> failwith "Syntax error"
            in
        let rec parse_raw str =
            match str with
                [] -> []
                | '"'::t -> let (s, tail) = get_string t in String s :: parse_raw tail
                | '\''::t -> let (c, tail) = get_char t in Char c :: parse_raw tail
                | c::t -> let res = parse_raw t in
                    match res with
                        Raw x :: t -> Raw (c::x) :: t
                    | t -> Raw [c] :: t
            in
        let rec go tok =
            match tok with
                [] -> []
                | Raw str :: t -> parse_raw str :: go t
                | h :: t -> [h] :: go t
        in List.flatten (go tokens)

    let string_of_chars chars = 
        let buf = Buffer.create 16 in
        List.iter (Buffer.add_char buf) chars;
        Buffer.contents buf

    let parse tokens =
        let char_to_int c = Char.code c - Char.code '0' in
        let final_touch token =
            match token with
                Id str -> (try Keyword (Hashtbl.find keywords str) with Not_found -> Id str)
                | Operator str when str = "=" -> Equality
                | Operator str when str = "->" -> Arrow
                | x -> x
                in
        let rec parse_raw prev str =
            match str, prev with
                [], None -> []
                | [], Some x -> [final_touch x]
                | h::t, _ ->
                match prev, char_type h with
                    None, Whitespace -> parse_raw None t
                    | None, Digit -> parse_raw (Some (Int (char_to_int h))) t
                    | None, Letter -> parse_raw (Some (Id (string_of_char h))) t
                    | None, Dot -> parse_raw (Some (Float (0.0, 1.0))) t
                    | None, Special1 -> special1_type h :: parse_raw None t
                    | None, Special2 -> parse_raw (Some (Operator (string_of_char h))) t
                    | Some x, Whitespace -> final_touch x :: parse_raw None t
                    | Some x, Special1 -> final_touch x :: special1_type h :: parse_raw None t
                    | Some (Operator str), Special2 -> parse_raw (Some (Operator (str ^ string_of_char h))) t
                    | Some (Operator str), _ -> final_touch (Operator str) :: parse_raw None (h::t)
                    | Some x, Special2 -> final_touch x :: parse_raw (Some (Operator (string_of_char h))) t
                    | Some (Int x), Digit -> parse_raw (Some (Int (x * 10 + char_to_int h))) t
                    | Some (Int x), Dot -> parse_raw (Some (Float (float_of_int x, 1.0))) t
                    | Some (Float (x, m)), Digit -> parse_raw (Some 
                        (Float(x +. float_of_int (char_to_int h) *. m /. 10.0, m /. 10.0))) t
                    | Some (Id x), Letter -> parse_raw (Some (Id (x ^ string_of_char h))) t
                    | Some (Id x), Digit -> parse_raw (Some (Id (x ^ string_of_char h))) t
                    | _ -> failwith ("Syntax error: invalid token in " ^ string_of_chars str)
            in
        let rec go tok =
            match tok with
                [] -> []
                | Raw str :: t -> parse_raw None str :: go t
                | h :: t -> [h] :: go t
        in List.flatten (go tokens)

    let classify_operators tok =
        let expr t =
            match t with
                Int _
              | Float (_, _)
              | String _
              | Char _
              | Id _
              | RightBracket -> true
              | _ -> false
            in
        let rec go prev tok =
            match tok with
                [] -> []
              | Operator x :: t -> (if prev then BinaryOperator x else UnaryOperator x) :: go false t
              | h :: t -> h :: go (expr h) t
        in go false tok

    let get_tokens str =
        let rec make_list i =
            if i = String.length str then [[]]
            else let c = String.get str i in
                if c = '\n' then []::make_list (i+1)
                else let h::t = make_list(i+1) in (c::h)::t
            in
        let text_parsed = parse_text (List.map (fun f -> Raw f) (make_list 0)) in
        classify_operators (parse text_parsed)
end

module type ProgramType =
sig
    type varID = TextID of string | IndexID of int * string
    type value = VInt of int 
               | VFloat of float 
               | VString of string 
               | VChar of char
               | VBool of bool
               | VUnit
               | VList of value list 
               | VTuple of value list
               | VFunction of pattern * expression
               | VUnOperator of (value -> value)
               | VBinOperator of (value -> value -> value)
    and expression = Constant of value
                   | Variable of varID
                   | Call of expression * expression
                   | Lambda of pattern * expression
                   | Tuple of expression list
                   | UnaryOperator of varID * expression
                   | BinaryOperator of varID * expression * expression
                   | IfElse of expression * expression * expression
                   | LetIn of definition_list * expression
    and definition = pattern * expression
    and definition_list = definition list
    and pattern = expression
    
    type statement = Definition of definition
                   | DefinitionList of definition_list
                   | Expression of expression
                   | RawTuple of expression list
                   | Raw of Token.t

    type t = { ids: (string, int) Hashtbl.t; values: value option Stack.t Vector.t }

    val create: unit -> t
    val parse: Token.t list -> statement
    val bind_ids: t -> statement -> statement
end

module Program : ProgramType =
struct
    type varID = TextID of string | IndexID of int * string
    type value = VInt of int 
               | VFloat of float 
               | VString of string 
               | VChar of char
               | VBool of bool
               | VUnit
               | VList of value list 
               | VTuple of value list
               | VFunction of pattern * expression
               | VUnOperator of (value -> value)
               | VBinOperator of (value -> value -> value)
    and expression = Constant of value
                   | Variable of varID
                   | Call of expression * expression
                   | Lambda of pattern * expression
                   | Tuple of expression list
                   | UnaryOperator of varID * expression
                   | BinaryOperator of varID * expression * expression
                   | IfElse of expression * expression * expression
                   | LetIn of definition_list * expression
    and definition = pattern * expression
    and definition_list = definition list
    and pattern = expression
    
    type statement = Definition of definition
                   | DefinitionList of definition_list
                   | Expression of expression
                   | RawTuple of expression list
                   | Raw of Token.t

    type t = { ids: (string, int) Hashtbl.t; values: value option Stack.t Vector.t }

    let create () = { ids = Hashtbl.create 50; values = Vector.create (Stack.create ()) }

    let parse tok =
        if tok = [] then failwith "Expression is empty" else
        let rec parse_atom tok =
            match tok with
                [] -> []
              | Token.Int x :: t -> Expression (Constant (VInt x)) :: parse_atom t
              | Token.Float (x, y) :: t -> Expression (Constant (VFloat x)) :: parse_atom t
              | Token.String x :: t -> Expression (Constant (VString x)) :: parse_atom t
              | Token.Char x :: t -> Expression (Constant (VChar x)) :: parse_atom t
              | Token.Id x :: t -> Expression (Variable (TextID x)) :: parse_atom t
              | h :: t -> Raw h :: parse_atom t
            in
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
                                    print_string op;
                                    print_newline ();
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
            in
        let left_brackets = [Token.LeftBracket; Token.Keyword "if"; Token.Keyword "let"; Token.Keyword "fun"]
        and separators = [Token.Keyword "then"; Token.Keyword "and"; Token.Equality]
        and right_brackets = [Token.RightBracket; Token.Keyword "else"; Token.Keyword "in"; Token.Arrow] in
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
            in
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
            in
        let dl = DList.of_list (parse_atom tok) in
        fold_brackets dl;
        fold_expr dl (DList._end dl);
        let open DList in
        fix_fun (get (get dl.next).item)

    let bind_ids program statement = (*TODO: tuple*)
        let rec bind_pattern (Variable v) =
            match v with
                IndexID (id, t) -> Hashtbl.add program.ids t id; Variable v
              | TextID t -> let id = Vector.length program.values in
                            Vector.append program.values (Stack.create());
                            Hashtbl.add program.ids t id; 
                            Variable (IndexID (id, t))
            in
        let rec unbind_pattern (Variable v) =
            let IndexID (_, t) = v in Hashtbl.remove program.ids t
            in
        let get_id id =
            match id with
                TextID text -> let nr = try Hashtbl.find program.ids text with _ -> failwith ("Unbound value " ^ text)
                               in IndexID (nr, text)
              | _ -> id
            in
        let bind_def = List.map (fun (p, e) -> (bind_pattern p, e)) in
        let unbind_def def = ignore(List.map (fun (p, _) -> unbind_pattern p) def) in
        let rec go_def def = List.map (fun (p, e) -> (p, go_expr e)) def
        and go_expr e =
            match e with
                  Variable id -> Variable (get_id id)
                | Call (a, b) -> Call (go_expr a, go_expr b)
                | Lambda (p, e) -> let p = bind_pattern p in
                                let e = go_expr e in
                                unbind_pattern p; Lambda (p, e)
                | Tuple t -> Tuple (List.map go_expr t)
                | UnaryOperator (id, e) -> UnaryOperator (get_id id, go_expr e)
                | BinaryOperator (id, a, b) -> BinaryOperator (get_id id, go_expr a, go_expr b)
                | IfElse (con, a, b) -> IfElse (go_expr con, go_expr a, go_expr b)
                | LetIn (def, expr) -> let def = bind_def def in
                                        let def = go_def def in
                                        let expr = go_expr expr in
                                        unbind_def def;
                                        LetIn (def, expr)
                | _ -> e
            in
        let go statement =
            match statement with
                Expression e -> Expression (go_expr e)
              | DefinitionList def -> let def = bind_def def in
                                      let def = go_def def in
                                      unbind_def def; DefinitionList def
              | _ -> failwith "Parsing failed"
        in go statement
end