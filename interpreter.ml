let string_of_char c = String.make 1 c

module type VectorType =
sig
    type 'a t

    val create: 'a -> 'a t
    val length: 'a t -> int
    val append: 'a t -> 'a -> unit
    val pop: 'a t -> unit
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
    
    let pop vec =
        vec.len <- vec.len - 1;
        Array.set vec.arr vec.len vec.default

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
            | LeftSBracket
            | RightSBracket
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

    exception Err of string

    val get_tokens: string -> t list
end

module Token : TokenType =
struct
    type t = LeftBracket 
            | RightBracket
            | LeftSBracket
            | RightSBracket
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

    exception Err of string

    type character = Digit | Letter | Whitespace | Dot | Special1 | Special2

    let failwith str = raise (Err str)

    let characters = [(Digit, "0123456789");
                    (Letter, "qwertyuiopasdfghjklzxcvbnmQWERTYUIOPASDFGHJKLZXCVBNM_");
                    (Whitespace, " \n\t\r");
                    (Dot, ".");
                    (Special1, "()[];,");
                    (Special2, "@$:~!%^&*-+=|<,>/")]

    let keywords =
        let rec make_hashtbl tbl l =
            match l with [] -> tbl | h::t -> Hashtbl.add tbl h h; make_hashtbl tbl t
        and keywords_list = ["let"; "in"; "and"; "if"; "then"; "else"; "fun"; "true"; "false"]
        in  make_hashtbl (Hashtbl.create 10) keywords_list

    let special1_type c = let specials = [('(', LeftBracket);
                                          (')', RightBracket);
                                          ('[', LeftSBracket);
                                          (']', RightSBracket); 
                                          (';', SemiColon);
                                          (',', Comma)] in
        List.assoc c specials

    let char_type c =
        try (let (result, _) =
            List.find (function (_, str) -> String.contains str c) characters in result)
        with Not_found -> failwith ("invalid_character: " ^ (string_of_char c))

    let parse_text tokens =
        let special = [('n', '\n');('t', '\t');('\\', '\\')] in
        let special_string, special_char = ('"', '"')::special, ('\'', '\'')::special in
        let get_string str =
            let buf = Buffer.create 1 in
            let rec go str =
                match str with
                    '\\'::c::t -> 
                        (try Buffer.add_char buf (List.assoc c special_string); go t
                        with Not_found -> failwith "wrong letter after backslash")
                | '"'::t -> t
                | c::t -> Buffer.add_char buf c; go t
                | _ -> failwith "expected \""
                in
            let tail = go str in (Buffer.contents buf, tail) in
        let get_char str =
            match str with
                '\\'::c::'\''::t -> (try (List.assoc c special_char, t)
                    with Not_found -> failwith "wrong letter after backslash")
                | c::'\''::t when c <> '\\' -> (c, t)
                | _ -> failwith "expected \'"
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
                    | _ -> failwith ("invalid token in " ^ string_of_chars str)
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
              | RightBracket
              | RightSBracket -> true
              | _ -> false
            in
        let rec go prev tok =
            match tok with
                [] -> []
              | Operator x :: t -> (if prev then BinaryOperator x else UnaryOperator (x ^ "u")) :: go false t
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
    type varID = TextID of string | GlobalID of int | LocalID of int | PatternID of int * string | JokerID
    type value = VInt of int 
               | VFloat of float 
               | VString of string 
               | VChar of char
               | VBool of bool
               | VUnit
               | VList of value list 
               | VTuple of value list
               | VFunction of pattern * expression * value option ref array
    and expression = Constant of value
                   | Variable of varID
                   | Call of expression * expression
                   | Lambda of pattern * expression
                   | LambdaFV of pattern * expression * varID array
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

    type t

    exception RuntimeErr of string
    exception BindErr of string
    exception InternalErr of string

    val create: unit -> t
    val bind_ids: t -> statement -> statement
    val eval: t -> expression -> value
    val feed: t -> definition_list -> (string * value) list
    val add_value: t-> string -> value -> unit
    val add_bin_operator: t -> string -> (value -> value -> value) -> unit
    val add_un_operator: t -> string -> (value -> value) -> unit
    val backup: t -> int
    val restore: t -> int -> unit
end

module Program : ProgramType =
struct
    type varID = TextID of string | GlobalID of int | LocalID of int | PatternID of int * string | JokerID
    type value = VInt of int 
               | VFloat of float 
               | VString of string 
               | VChar of char
               | VBool of bool
               | VUnit
               | VList of value list 
               | VTuple of value list
               | VFunction of pattern * expression * value option ref array
    and expression = Constant of value
                   | Variable of varID
                   | Call of expression * expression
                   | Lambda of pattern * expression
                   | LambdaFV of pattern * expression * varID array
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

    type t = { ids: (string, int) Hashtbl.t;
               values: value option ref Stack.t Vector.t; 
               bin_ops: (value -> value -> value) Vector.t;
               un_ops: (value -> value) Vector.t }

    exception RuntimeErr of string
    exception BindErr of string
    exception InternalErr of string

    let fail_runtime str = raise (InternalErr str)
    let fail_bind str = raise (BindErr str)
    let fail_internal str = raise (RuntimeErr str)

    let create () = { ids = Hashtbl.create 50;
                      values = Vector.create (Stack.create ());
                      bin_ops = Vector.create (fun _ _ -> VUnit);
                      un_ops = Vector.create (fun _ -> VUnit) }

    let find_free_variables statement =
        let (module M) = (module Set.Make(Int32) : Set.S with type elt = int32) in
        let i32 = Int32.of_int in
        let union_list = List.fold_left M.union M.empty in
        let rec go_expr e =
            match e with
                Variable (GlobalID id) -> (e, M.singleton (i32 id))
              | Lambda (p, e) -> let fp = go_pat p and (e, fe) = go_expr e in
                                 let fv = M.diff fe fp in
                                 (LambdaFV (p, e, fv |> M.elements |> List.map (fun x -> GlobalID (Int32.to_int x))
                                  |> Array.of_list), fv)
              | Call (a, b) -> let (a, fa) = go_expr a and (b, fb) = go_expr b in
                                           (Call (a, b), M.union fa fb)
              | Tuple t -> let l = List.map go_expr t in (Tuple (List.map (fun (x, _) -> x) l),
                                                          List.map (fun (_, x) -> x) l |> union_list)
              | UnaryOperator (id, x) -> let (x, fx) = go_expr x in (UnaryOperator (id, x), fx)
              | BinaryOperator (id, a, b) -> let (a, fa) = go_expr a and (b, fb) = go_expr b in
                                            (BinaryOperator (id, a, b), M.union fa fb)
              | IfElse (con, a, b) -> let (con, fc) = go_expr con and (a, fa) = go_expr a and (b, fb) = go_expr b in
                                            (IfElse (con, a, b), M.union (M.union fa fb) fc)
              | LetIn (def, expr) -> let (def, fp, fv) = go_def def and (expr, fe) = go_expr expr in
                                     (LetIn (def, expr), M.diff (M.union fe fv) fp)
              | _ -> (e, M.empty)
        and go_pat pat =
            match pat with
                Variable (PatternID (id, _)) -> M.singleton (i32 id)
              | Tuple t -> t |> List.map go_pat |> union_list
              | BinaryOperator (_, a, b) -> M.union (go_pat a) (go_pat b)
              | _ -> M.empty
        and go_def def =
            let (p, e) = List.split def in
            let fp = p |> List.map go_pat |> union_list in
            let (e, fe) = e |> List.map go_expr |> List.split in
            (List.combine p e, fp, union_list fe)
            in
                    
        match statement with
            Expression e -> let (e, _) = go_expr e in Expression e
          | DefinitionList def -> let (def, _, _) = go_def def in DefinitionList def
          | _ -> fail_internal "find_free_variables"

    let fix_local_ids statement =
        let get_id tbl vid =
            match vid with
                GlobalID id -> (try let local = Hashtbl.find tbl id in LocalID local
                                with Not_found -> vid)
            | _ -> vid
            in
        let rec go_expr tbl e =
            match e with
                Variable v -> Variable (get_id tbl v)
              | LambdaFV (p, e, fv) ->
                    let nfv = Array.map (get_id tbl) fv in
                    let ntbl = Hashtbl.create (Array.length fv) in
                    ignore (Array.mapi (fun i (GlobalID id) -> Hashtbl.add ntbl id i) fv);
                    LambdaFV (p, go_expr ntbl e, nfv)
              | Call (a, b) -> Call (go_expr tbl a, go_expr tbl b)
              | Tuple t -> Tuple (List.map (go_expr tbl) t)
              | UnaryOperator (id, x) -> UnaryOperator (id, go_expr tbl x)
              | BinaryOperator (id, a, b) -> BinaryOperator (id, go_expr tbl a, go_expr tbl b)
              | IfElse (con, a, b) -> IfElse (go_expr tbl con, go_expr tbl a, go_expr tbl b)
              | LetIn (def, expr) -> LetIn (go_def tbl def, go_expr tbl expr)
              | _ -> e
        and go_def tbl def = List.map (fun (p, e) -> (p, go_expr tbl e)) def in
        let tbl = Hashtbl.create 10 in
        match statement with
            Expression e -> Expression (go_expr tbl e)
          | DefinitionList def -> DefinitionList (go_def tbl def)
          | _ -> fail_internal "fix_local_ids"

    let bind_ids program statement =
        let rec bind_pattern pat =
            match pat with
                Variable v ->
                    begin match v with
                        PatternID (id, t) -> Hashtbl.add program.ids t id; Variable v
                      | TextID "_" -> Variable (JokerID)
                      | TextID t -> let id = Vector.length program.values in
                                    Vector.append program.values (Stack.create());
                                    Hashtbl.add program.ids t id; 
                                    Variable (PatternID (id, t))
                      | _ -> Variable v
                    end
              | Tuple t -> Tuple (List.map bind_pattern t)
              | BinaryOperator (id, a, b) -> BinaryOperator (id, bind_pattern a, bind_pattern b)
              | _ -> pat
            in
        let rec unbind_pattern pat =
            match pat with
                Variable (PatternID (_, t)) -> Hashtbl.remove program.ids t
              | Variable (JokerID) -> ()
              | Tuple t -> ignore (List.map unbind_pattern t)
              | BinaryOperator (_, a, b) -> unbind_pattern a; unbind_pattern b
              | _ -> ()
            in
        let get_id id =
            match id with
                TextID text -> let nr = try Hashtbl.find program.ids text with _ -> fail_bind text
                               in GlobalID nr
              | _ -> id
            in
        let bind_def = List.map (fun (p, e) -> (bind_pattern p, e)) in
        let unbind_def def = List.iter (fun (p, _) -> unbind_pattern p) def in
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
        (match statement with
              Expression e -> Expression (go_expr e)
            | DefinitionList def -> let def = bind_def def in
                                    let def = go_def def in
                                    unbind_def def; DefinitionList def
            | _ -> fail_internal "bind_ids")
        |> find_free_variables
        |> fix_local_ids

    let rec push_pattern program pat =
        match pat with
            Variable (PatternID (id, _)) -> Vector.get program.values id |> Stack.push (ref None)
          | Variable (JokerID) -> ()
          | Tuple t -> List.iter (push_pattern program) t
          | BinaryOperator (_, a, b) -> push_pattern program a; push_pattern program b
          | _ -> ()

    let rec pop_pattern program pat =
        match pat with
            Variable (PatternID (id, _)) -> ignore(Vector.get program.values id |> Stack.pop)
          | Tuple t -> List.iter (pop_pattern program) t
          | BinaryOperator (_, a, b) -> pop_pattern program a; pop_pattern program b
          | _ -> ()

    let rec get_variables pat =
        match pat with
            Variable (PatternID (a, b)) -> [PatternID (a, b)]
          | Tuple t -> List.map get_variables t |> List.flatten
          | BinaryOperator (_, a, b) -> get_variables a @ get_variables b
          | _ -> []
          
    let rec check_pattern pat value =
        match pat, value with
            Tuple t, VTuple v when List.length t = List.length v ->
                List.fold_left2 (fun x y z -> x && (check_pattern y z)) true t v
          | Constant c, _ when c = value -> true
          | BinaryOperator (TextID "::", h, t), VList (vh::vt) -> check_pattern h vh && check_pattern t (VList vt)
          | Variable _, _ -> true
          | _ -> false

    let assign_pattern program pat value =
        let rec go pat value =
            match pat, value with
                Variable (PatternID (id, _)), _ -> (Vector.get program.values id |> Stack.top) := Some value
              | Tuple t, VTuple v -> List.iter2 go t v
              | BinaryOperator (TextID "::", v, t), VList (vh::vt) ->
                    go v vh;
                    go t (VList vt)
              | _ -> ()
            in
        go pat value
    
    let try_assign_pattern program pat value =
        if check_pattern pat value then assign_pattern program pat value else fail_runtime "pattern matching failed"

    let eval program e =
        let get_value_ref local id =
            match id with
                GlobalID id -> Stack.top (Vector.get program.values id)
              | LocalID id -> local.(id)
              | _ -> fail_internal "get_value_ref"
            in
        let get_value local id =
            match !(get_value_ref local id) with
                Some x -> x
              | None -> fail_runtime "trying to access value of uninitialised variable"
            in
        let rec go local expr =
            match expr with
                Constant c -> c
              | Variable id -> get_value local id
              | Call (f, a) ->
                begin
                    let arg = go local a in
                    match go local f with
                        VFunction (pat, body, loc) ->
                            push_pattern program pat;
                            try_assign_pattern program pat arg;
                            let result = go loc body in
                            pop_pattern program pat;
                            result
                      | _ -> fail_runtime "type error: expected function"
                end
              | Tuple t -> VTuple (List.map (go local) t)
              | LambdaFV (pat, body, loc) -> VFunction (pat, body, Array.map (get_value_ref local) loc)
              | UnaryOperator ((GlobalID id), x) -> let f = Vector.get program.un_ops id in f (go local x)
              | BinaryOperator ((GlobalID id), a, b) -> let f = Vector.get program.bin_ops id in 
                                                        (try f (go local a) (go local b) with _ ->
                                                             fail_runtime "type error: wrong type for operator")
              | IfElse (con, a, b) ->
                begin
                    match go local con with
                        VBool true -> go local a
                      | VBool false -> go local b
                      | _ -> fail_runtime "type error: expected bool in if clausule"
                end
              | LetIn (def, expr) ->
                    let pats, _ = List.split def in
                    List.iter (push_pattern program) pats;
                    List.rev def |> List.iter (fun (p, v) -> try_assign_pattern program p (go local v));
                    let res = go local expr in
                    List.iter (pop_pattern program) pats;
                    res
              | _ -> fail_internal "eval"
            in
        go [||] e

    let feed program def =
        let pats, _ = List.split def in
        List.iter (push_pattern program) pats;
        List.rev def |> List.iter (fun (p, v) -> try_assign_pattern program p (eval program v));
        let get_value id = let Some x = !(Vector.get program.values id |> Stack.top) in x in
        List.map get_variables pats |> List.flatten |> 
            List.map (fun (PatternID (id, name)) -> Hashtbl.add program.ids name id; (name, get_value id))

    let add_value program id v =
        let nr = Vector.length program.values in
        Hashtbl.add program.ids id nr;
        Vector.append program.values (Stack.create ());
        Stack.push (ref (Some v)) (Vector.get program.values nr)

    let add_bin_operator program id f =
        let nr = Vector.length program.bin_ops in
        Hashtbl.add program.ids id nr;
        Vector.append program.bin_ops f
    
    let add_un_operator program id f =
        let id = id ^ "u" in
        let nr = Vector.length program.un_ops in
        Hashtbl.add program.ids id nr;
        Vector.append program.un_ops f
        
    let backup program = Vector.length program.values
    let restore program nr =
        Hashtbl.iter (fun a b -> if b >= nr then Hashtbl.remove program.ids a) program.ids;
        while Vector.length program.values > nr do
            Vector.pop program.values
        done
end

module type ParserType =
sig
    exception Err of string
    val parse: Token.t list -> Program.statement
end

module Parser : ParserType =
struct
    open Program

    exception Err of string
    let failwith str = raise (Err str)

    let rec parse_atom tok =
        match tok with
            [] -> []
            | Token.Int x :: t -> Expression (Constant (VInt x)) :: parse_atom t
            | Token.Float (x, y) :: t -> Expression (Constant (VFloat x)) :: parse_atom t
            | Token.String x :: t -> Expression (Constant (VString x)) :: parse_atom t
            | Token.Char x :: t -> Expression (Constant (VChar x)) :: parse_atom t
            | Token.Keyword "true" :: t -> Expression (Constant (VBool true)) :: parse_atom t
            | Token.Keyword "false" :: t -> Expression (Constant (VBool false)) :: parse_atom t
            | Token.Id x :: t -> Expression (Variable (TextID x)) :: parse_atom t
            | h :: t -> Raw h :: parse_atom t
        
    let rec make_list l =
        match l with
            [] -> Constant (VList [])
          | h::t -> BinaryOperator (TextID "::", h, make_list t)

    let rec fold_expr bg en =
        let open DList in
        if not (equal (get bg.next) en) && not (equal (get bg.next) (get en.prev)) then
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
            while not (equal !it en) do
                begin match (get !it.prev).item, !it.item with
                    Some (Raw (Token.LeftBracket)), Some (Raw (Token.RightBracket)) ->
                        ignore (remove (get !it.prev));
                        !it.item <- Some (Expression (Constant (VUnit)))
                    | Some (Raw (Token.LeftSBracket)), Some (Raw (Token.RightSBracket)) ->
                        ignore (remove (get !it.prev));
                        !it.item <- Some (Expression (Constant (VList [])))
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
                    | Some (RawTuple t), Some (Raw (Token.LeftBracket)), Some (Raw (Token.RightBracket)) ->
                        ignore (remove (get !it.next));
                        ignore (remove (get !it.prev));
                        !it.item <- Some (Expression (Tuple t))
                    | Some (RawTuple t), Some (Raw (Token.LeftSBracket)), Some (Raw (Token.RightSBracket)) ->
                        ignore (remove (get !it.next));
                        ignore (remove (get !it.prev));
                        !it.item <- Some (Expression (make_list t))
                    | Some (Expression e), Some (Raw (Token.LeftSBracket)), Some (Raw (Token.RightSBracket)) ->
                        ignore (remove (get !it.next));
                        ignore (remove (get !it.prev));
                        !it.item <- Some (Expression (make_list [e]))
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
                                else if equal it en then failwith "unexpected end of expression"
                                else let ret = get it.item :: cut_n (get it.next) (n-1) in
                                        remove it; ret
                            in let l = cut_n (get !it.next) comp.argc
                            in !it.item <- Some (comp.func l)
                        with Not_found -> () | _ -> failwith "syntax error"
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
            let fold_bin_rev filter =
                let it = ref (get (get en.prev).prev) in
                while not (equal !it bg) && not (equal !it (get bg.next)) do
                    begin match !it.item, (get !it.prev).item, (get !it.next).item with
                        Some (Raw (Token.BinaryOperator op)), Some (Expression a), Some (Expression b)
                            when filter op ->
                                !it.item <- Some (Expression (BinaryOperator (TextID op, a, b)));
                                ignore (remove (get !it.next));
                                ignore (remove (get !it.prev))
                        | _ -> ()
                    end;
                    it := get !it.prev
                done
                in
            let list_filter l x = List.mem x l in
            fold_bin (list_filter ["**"]);
            fold_bin (list_filter ["*"; "/"; "%"]);
            fold_bin (list_filter ["+"; "-"]);
            fold_bin (list_filter ["^";"|";"&"]);
            fold_bin (list_filter ["==";">";"<";">=";"<="]);
            fold_bin (list_filter ["||";"&&"]);
            fold_bin_rev (list_filter ["::"]);
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
            done;

            if not (equal (get bg.next) (get en.prev)) then failwith "syntax error"
        end

    let left_brackets = [Token.LeftBracket; Token.LeftSBracket; Token.Keyword "if";
                         Token.Keyword "let"; Token.Keyword "fun"]
    let separators = [Token.Keyword "then"; Token.Keyword "and"; Token.Equality]
    let right_brackets = [Token.RightBracket; Token.RightSBracket;
                          Token.Keyword "else"; Token.Keyword "in"; Token.Arrow]
    let fold_brackets dl =
        let open DList in
        let it = ref (get dl.next) in
        let st = Stack.create () in
        while !it.item <> None do
            begin match !it.item with
                Some (Raw x) ->
                    if List.mem x separators || List.mem x right_brackets then begin
                        let prev = try Stack.pop st with Stack.Empty -> failwith "syntax error" in 
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
        let rec check pat =
            match pat with
                Constant _ | Variable _ | Tuple _ | BinaryOperator (TextID "::", _, _) -> ()
              | _ -> failwith "invalid pattern"
            in
        let rec go pat f =
            match pat with
                Call(p, e) -> go p (Lambda (e, f))
              | _ -> check pat; (pat, f)
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
                                            DefinitionList (go p (go_expr e) :: t)
            | Expression e -> Expression (go_expr e)
            | _ -> failwith "syntax error"

    let parse tok =
        if tok = [] then failwith "empty expression" else
        let open DList in
        let dl = of_list (parse_atom tok) in
        fold_brackets dl;
        fold_expr dl (_end dl);
        fix_fun (get (get dl.next).item)
end

let init_operators program =
    let open Program in
    add_bin_operator program "+" (fun a b ->
        match a, b with
            VInt a, VInt b -> VInt (a + b)
          | VFloat a, VFloat b -> VFloat (a +. b)
          | VString a, VString b -> VString (a ^ b)
          | VList a, VList b -> VList (a @ b)
          | _ -> failwith "wrong types");
    add_bin_operator program "-" (fun a b ->
        match a, b with
            VInt a, VInt b -> VInt (a - b)
          | VFloat a, VFloat b -> VFloat (a -. b)
          | _ -> failwith "wrong types");
    add_bin_operator program "*" (fun a b ->
        match a, b with
            VInt a, VInt b -> VInt (a * b)
          | VFloat a, VFloat b -> VFloat (a *. b)
          | _ -> failwith "wrong types");
    add_bin_operator program "/" (fun a b ->
        match a, b with
            VInt a, VInt b when b <> 0 -> VInt (a / b)
          | VFloat a, VFloat b -> VFloat (a /. b)
          | _ -> failwith "wrong types");
    add_bin_operator program "%" (fun a b ->
        match a, b with
            VInt a, VInt b when b <> 0 -> VInt (a mod b)
          | _ -> failwith "wrong types");
    add_bin_operator program "**" (fun a b ->
        match a, b with
            VInt a, VInt b -> VFloat (float_of_int a ** float_of_int b)
          | VFloat a, VFloat b -> VFloat (a ** b)
          | _ -> failwith "wrong types");
    add_bin_operator program ">" (fun a b -> VBool (a > b));
    add_bin_operator program "<" (fun a b -> VBool (a < b));
    add_bin_operator program ">=" (fun a b -> VBool (a >= b));
    add_bin_operator program "<=" (fun a b -> VBool (a <= b));
    add_bin_operator program "==" (fun a b -> VBool (a = b));
    add_bin_operator program "!=" (fun a b -> VBool (a <> b));
    add_bin_operator program "||" (fun a b ->
        match a, b with
            VBool false, VBool false -> VBool false
          | VBool _, VBool _ -> VBool true
          | _ -> failwith "wrong types");
    add_bin_operator program "&&" (fun a b ->
        match a, b with
            VBool true, VBool true -> VBool true
          | VBool _, VBool _ -> VBool false
          | _ -> failwith "wrong types");
    add_bin_operator program "|" (fun a b ->
        match a, b with
            VInt a, VInt b -> VInt (a lor b)
          | _ -> failwith "wrong types");
    add_bin_operator program "&" (fun a b ->
        match a, b with
            VInt a, VInt b -> VInt (a land b)
          | _ -> failwith "wrong types");
    add_bin_operator program "<<" (fun a b ->
        match a, b with
            VInt a, VInt b -> VInt (a lsl b)
          | _ -> failwith "wrong types");
    add_bin_operator program ">>" (fun a b ->
        match a, b with
            VInt a, VInt b -> VInt (a lsr b)
          | _ -> failwith "wrong types");
    add_bin_operator program "<<" (fun a b ->
        match a, b with
            VInt a, VInt b -> VInt (a lsl b)
          | _ -> failwith "wrong types");
    add_bin_operator program "::" (fun a b ->
        match a, b with
            _, VList b -> VList (a :: b)
          | _ -> failwith "wrong types");
    add_un_operator program "-" (fun x ->
        match x with
            VInt x -> VInt (-x)
          | VFloat x -> VFloat (-.x)
          | _ -> failwith "wrong types");
    add_un_operator program "!" (fun x ->
        match x with
            VBool x -> VBool (not x)
          | _ -> failwith "wrong types");
    add_un_operator program "~" (fun x ->
        match x with
            VInt x -> VInt (lnot x)
          | _ -> failwith "wrong types")

module REPL =
struct

    let split_on_semicolon l =
        let rec go l p =
            match l with
                [] -> []
              | Token.SemiColon :: t ->
                    let t = go t [] in if p = [] then t else p :: t
              | h :: t -> go t (h :: p)
        in go l [] |> List.map List.rev
    
    let eval_statement program l =
        let backup = Program.backup program in
        try
            let statement = l |> Parser.parse |> Program.bind_ids program in 
            match statement with
                Program.Expression e -> Program.eval program e; print_endline "eval"; ()
              | Program.DefinitionList def -> Program.feed program def; print_endline "feed"; ()
        with err ->
            Program.restore program backup;
            raise err

    let eval_code program str =
        try
            String.split_on_char '\n' str
         |> List.map Token.get_tokens
         |> List.flatten
         |> split_on_semicolon
         |> List.iter (eval_statement program)
        with Token.Err err -> print_endline ("Lexer error: " ^ err)
           | Parser.Err err -> print_endline ("Parser error: " ^ err)
           | Program.RuntimeErr err -> print_endline ("Runtime error: " ^ err)
           | Program.BindErr err -> print_endline ("Unbound value: " ^ err)
           | Program.InternalErr err -> print_endline ("Internal error: " ^ err)
           | _ -> print_endline "Other error"

end