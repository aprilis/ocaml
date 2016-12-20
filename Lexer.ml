let string_of_char c = String.make 1 c

type token = LeftBracket 
            | RightBracket
            | SemiColon
            | Equality
            | Keyword of string
            | Operator of string
            | TInt of int
            | TFloat of float * float
            | TString of string
            | TChar of char
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
            | '"'::t -> let (s, tail) = get_string t in TString s :: parse_raw tail
            | '\''::t -> let (c, tail) = get_char t in TChar c :: parse_raw tail
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
            | x -> x
            in
    let rec parse_raw prev str =
        match str, prev with
            [], None -> []
            | [], Some x -> [final_touch x]
            | h::t, _ ->
            match prev, char_type h with
                None, Whitespace -> parse_raw None t
                | None, Digit -> parse_raw (Some (TInt (char_to_int h))) t
                | None, Letter -> parse_raw (Some (Id (string_of_char h))) t
                | None, Dot -> parse_raw (Some (TFloat (0.0, 1.0))) t
                | None, Special1 -> special1_type h :: parse_raw None t
                | None, Special2 -> parse_raw (Some (Operator (string_of_char h))) t
                | Some x, Whitespace -> final_touch x :: parse_raw None t
                | Some x, Special1 -> final_touch x :: special1_type h :: parse_raw None t
                | Some (Operator str), Special2 -> parse_raw (Some (Operator (str ^ string_of_char h))) t
                | Some (Operator str), _ -> final_touch (Operator str) :: parse_raw None (h::t)
                | Some x, Special2 -> final_touch x :: parse_raw (Some (Operator (string_of_char h))) t
                | Some (TInt x), Digit -> parse_raw (Some (TInt (x * 10 + char_to_int h))) t
                | Some (TInt x), Dot -> parse_raw (Some (TFloat (float_of_int x, 1.0))) t
                | Some (TFloat (x, m)), Digit -> parse_raw (Some 
                    (TFloat(x +. float_of_int (char_to_int h) *. m /. 10.0, m /. 10.0))) t
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

let tokenize str =
    let rec make_list i =
        if i = String.length str then [[]]
        else let c = String.get str i in
            if c = '\n' then []::make_list (i+1)
            else let h::t = make_list(i+1) in (c::h)::t
        in
    let text_parsed = parse_text (List.map (fun f -> Raw f) (make_list 0)) in
    parse text_parsed