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
    type 'a t = { mutable prev: 'a t option; mutable next: 'a t option; mutable item: 'a option }

    let get (Some x) = x

    let create () =
        let rec a = { prev = None; next = Some b; item = None }
            and b = { prev = Some a; next = None; item = None }
        in b
    let remove x =
        (get x.prev).next <- x.next;
        (get x.next).prev <- x.prev;
        get x.next

    let insert_before a l =
        let it = { prev = l.prev; next = Some l; item = a } in
        (get l.prev).next <- Some it;
        l.prev <- Some it;
        it

    let rec of_list l =
        match l with
            [] -> create ()
          | h::t -> insert_before h (of_list l)
end

let (++) = DList.insert_before

module type TokenType =
sig
    type t = LeftBracket 
            | RightBracket
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
                    (Special1, "();");
                    (Special2, "@$:~!%^&*-+=[]|<,>/")]

    let keywords =
        let rec make_hashtbl tbl l =
            match l with [] -> tbl | h::t -> Hashtbl.add tbl h h; make_hashtbl tbl t
        and keywords_list = ["let"; "in"; "when"; "and"; "with"; "if"; "then"; "else"]
        in  make_hashtbl (Hashtbl.create 10) keywords_list

    let special1_type c = let specials = [('(', LeftBracket); (')', RightBracket); (';', SemiColon)] in
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
                Int _ -> true
              | Float (_, _) -> true
              | String _ -> true
              | Char _ -> true
              | Id _ -> true
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
        parse text_parsed
end

module Program =
struct
    type varID = TextID of string | IndexID of int
    type value = VInt of int 
               | VFloat of float 
               | VString of string 
               | VChar of char
               | VBool of bool 
               | VTuple of value array
               | VFunction of pattern * expression
    and expression = Constant of value
                   | Variable of varID
                   | Function of expression * expression
                   | UnaryOperator of varID * expression
                   | BinaryOperator of varID * expression * expression
                   | IfElse of expression * expression * expression
                   | LetIn of definition_list * expression
    and definition = pattern * expression
    and definition_list = definition list
    and pattern = varID
    
    type statement = Definition of definition
                   | DefinitionList of definition_list
                   | Expression of expression
                   | Pattern of pattern
                   | Raw of Token.t

    type t = { ids: (string, varID) Hashtbl.t; values: value Stack.t Vector.t }

    let parse tok =
        let rec parse_atom tok =
            match tok with
                [] -> []
              | Token.Int x :: t -> Expression (Constant (VInt x)) :: parse_atom tok
              | Token.Float (x, y) :: t -> Expression (Constant (VFloat x)) :: parse_atom tok
              | Token.String x :: t -> Expression (Constant (VString x)) :: parse_atom tok
              | Token.Char x :: t -> Expression (Constant (VChar x)) :: parse_atom tok
              | Token.Id x :: t -> Expression (Variable (TextID x)) :: parse_atom tok
              | h :: t -> Raw h :: parse_atom tok
            in
        let brackets = [(Token.LeftBracket, Token.RightBracket); (Token.Keyword "let", Token.Equality);
                        (Token.Equality, Token.Keyword "in"); (Token.Keyword "if", Token.Keyword "then");
                        (Token.Keyword "then", Token.Keyword "else")] in
        let rec fold_expr bg en =
            if bg != en then
                let open DList in
                let bg, en = get bg.prev, get en.next in
                
                (* fold after keywords else, in, -> *)
                let fold_after = [Token.Keyword "else"; Token.Keyword "in"; Token.Arrow] in
                let it = ref (get bg.next) in
                while !it != en do
                    begin match !it.item with
                        Some (Raw token) when List.mem token fold_after ->
                            fold_expr !it en
                      | _ -> ()
                    end;
                    it := get !it.next
                done;

                (* brackets *)
                let it = ref (get (get bg.next).next) in
                while !it != en && !it != get en.prev do
                    begin match !it.item, (get !it.prev).item, (get !it.next).item with
                        Some (Expression e), Some (Raw (Token.LeftBracket)), Some (Raw (Token.RightBracket)) ->
                            ignore (remove (get !it.next));
                            ignore (remove (get !it.prev))
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
                                ("fun", { func = (fun [Pattern pat;
                                                       Raw Token.Arrow;
                                                       Expression e] ->
                                                       Expression (Constant (VFunction (pat, e))));
                                                       argc = 3});
                                ("let", { func = (fun [Pattern pat;
                                                       Raw Token.Equality;
                                                       Expression e] ->
                                                       DefinitionList [(pat, e)]); argc = 3});
                                ("and", { func = (fun [Pattern pat;
                                                       Raw Token.Equality;
                                                       Expression e] ->
                                                       Definition (pat, e)); argc = 3})] in
                let it = ref (get bg.next) in
                while !it != en do
                    begin match !it.item with
                        Some (Raw (Token.Keyword key)) ->
                            begin try let comp = List.assoc key comps in
                                let rec cut_n it n =
                                    if n = 0 then []
                                    else if it == en then failwith "Parse error"
                                    else let ret = get it.item :: cut_n (get it.next) (n-1) in
                                         remove it; ret
                                in let l = cut_n (get !it.next) comp.argc
                                in !it.item <- Some (comp.func l)
                            with Not_found -> () | _ -> failwith "Parse error"
                            end
                      | _ -> ()
                    end;
                    it := get !it.next
                done;

                let it = ref (get (get bg.next).next) in
                while !it != en do
                    begin match !it.item, (get !it.prev).item with
                        Some (Definition d), Some (DefinitionList l) ->
                            !it.item <- Some (DefinitionList (d :: l));
                            ignore (remove (get !it.prev))
                      | Some (Raw (Token.Keyword "in")), Some (DefinitionList d) ->
                        begin
                            let next = get !it.next in
                            if next != en then
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
                while !it != en && !it != get en.prev do
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
                while !it != bg do
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
                while !it != en do
                    begin match (get !it.prev).item, !it.item with
                        Some (Expression a), Some (Expression b) ->
                            !it.item <- Some (Expression (Function (a, b)));
                            ignore (remove (get !it.prev))
                      | _ -> ()
                    end;
                    it := get !it.next
                done;

                (* binary operators *)
                let fold_bin filter =
                    let it = ref (get (get bg.next).next) in
                    while !it != en && !it != get en.prev do
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
                fold_bin (fun x -> true);
                if bg.next != en.prev then failwith "Parse error"
            in ()

end