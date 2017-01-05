type varID = TextID of string | GlobalID of int | LocalID of int | PatternID of int * string | JokerID
type mtype = TInt | TFloat | TString | TChar | TBool | TUnit | TList | TFunction | TType | TAny
type value = VInt of int 
            | VFloat of float 
            | VString of string 
            | VChar of char
            | VBool of bool
            | VUnit
            | VList of value list 
            | VTuple of value list
            | VFunction of pattern * expression * value option ref array
            | VNative of (value -> value)
            | VType of mtype
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
                | MatchWith of expression * definition_list
and definition = pattern * expression
and definition_list = definition list
and pattern = expression

type statement = Definition of definition
                | DefinitionList of definition_list
                | Match of pattern * expression
                | Expression of expression
                | Import of string
                | Quit
                | RawTuple of expression list
                | Raw of Token.t

type t = { ids: (string, int) Hashtbl.t;
            values: value option ref Stack.t Vector.t; 
            bin_ops: (value -> value -> value) Vector.t;
            un_ops: (value -> value) Vector.t }

exception RuntimeErr of string
exception BindErr of string
exception InternalErr of string

let fail_runtime str = raise (RuntimeErr str)
let fail_bind str = raise (BindErr str)
let fail_internal str = raise (InternalErr str)

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
            | MatchWith (e, l) -> let (e, fe) = go_expr e and (l, fl) = go_mat l in
                                    (MatchWith (e, l), M.union fe fl)
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
    and go_mat l =
        let (p, e) = List.split l in
        let fp = p |> List.map go_pat in
        let (e, fe) = e |> List.map go_expr |> List.split in
        (List.combine p e, List.map2 M.diff fe fp |> union_list)
        in
                
    match statement with
        Expression e -> let (e, _) = go_expr e in Expression e
        | DefinitionList def -> let (def, _, _) = go_def def in DefinitionList def
        | Quit | Import _ -> statement
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
            | MatchWith (e, l) -> MatchWith (go_expr tbl e, go_def tbl l)
            | _ -> e
    and go_def tbl def = List.map (fun (p, e) -> (p, go_expr tbl e)) def in
    let tbl = Hashtbl.create 10 in
    match statement with
        Expression e -> Expression (go_expr tbl e)
        | DefinitionList def -> DefinitionList (go_def tbl def)
        | Quit | Import _ -> statement
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
    and go_mat l =
        let f (p, e) = let p = bind_pattern p in
                       let e = go_expr e in
                       unbind_pattern p; (p, e)
        in List.map f l
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
            | MatchWith (e, l) -> MatchWith (go_expr e, go_mat l)
            | _ -> e
        in
    (match statement with
            Expression e -> Expression (go_expr e)
        | DefinitionList def -> let def = bind_def def in
                                let def = go_def def in
                                unbind_def def; DefinitionList def
        | Quit | Import _ -> statement
        | _ -> fail_internal "bind_ids")
    |> find_free_variables
    |> fix_local_ids

let rec typeof value =
    match value with
        VInt _ -> VType TInt
      | VFloat _ -> VType TFloat
      | VChar _ -> VType TChar
      | VString _ -> VType TString
      | VBool _ -> VType TBool
      | VUnit -> VType TUnit
      | VList _ -> VType TList
      | VTuple t -> VTuple (List.map typeof t)
      | VFunction (_, _, _)
      | VNative _ -> VType TFunction 
      | VType _ -> VType TType

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
        
let rec check_type t1 t2 =
    match t1, t2 with
        VType TAny, _
      | _, VType TAny -> true
      | VType a, VType b when a = b -> true
      | VTuple a, VTuple b -> List.map2 check_type a b |> List.fold_left (&&) true
      | _ -> false

let rec check_pattern pat value =
    match pat, value with
          Tuple t, VTuple v when List.length t = List.length v ->
            List.fold_left2 (fun x y z -> x && (check_pattern y z)) true t v
        | Constant c, _ when c = value -> true
        | BinaryOperator (TextID "::", h, t), VList (vh::vt) -> check_pattern h vh && check_pattern t (VList vt)
        | BinaryOperator (TextID ":", a, Constant b), _ -> check_pattern a value && check_type b (typeof value)
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
            | BinaryOperator (TextID ":", p, _), _ -> go p value
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
                  | VNative f -> f (go local a)
                  | _ -> fail_runtime "type error: expected function"
            end
            | Tuple t -> VTuple (List.map (go local) t)
            | LambdaFV (pat, body, loc) -> VFunction (pat, body, Array.map (get_value_ref local) loc)
            | UnaryOperator ((GlobalID id), x) -> let f = Vector.get program.un_ops id and x = go local x in
                                                    (try f x with _ ->
                                                            fail_runtime "wrong argument for unary operator")
            | BinaryOperator ((GlobalID id), a, b) -> let f = Vector.get program.bin_ops id in
                                                      let a = go local a and b = go local b in
                                                    (try f a b with _ ->
                                                            fail_runtime "wrong arguments for binary operator")
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
                List.iter (fun (p, v) -> try_assign_pattern program p (go local v)) def;
                let res = go local expr in
                List.iter (pop_pattern program) pats;
                res
            | MatchWith (e, l) ->
                let e = go local e in
                let (p, r) = try List.find (fun (p,_) -> check_pattern p e) l with Not_found ->
                    fail_runtime "pattern matching failed" in
                push_pattern program p;
                assign_pattern program p e;
                let res = go local r in
                pop_pattern program p;
                res
            | _ -> fail_internal "eval"
        in
    go [||] e

let feed program def =
    let pats, _ = List.split def in
    List.iter (push_pattern program) pats;
    List.iter (fun (p, v) -> try_assign_pattern program p (eval program v)) def;
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
    Hashtbl.iter (fun a b -> if String.contains Token.letters a.[0] && b >= nr 
                    then Hashtbl.remove program.ids a) program.ids;
    while Vector.length program.values > nr do
        Vector.pop program.values
    done

let init program =
    add_bin_operator program "+" (fun a b ->
        match a, b with
            VInt a, VInt b -> VInt (a + b)
          | VFloat a, VFloat b -> VFloat (a +. b)
          | VInt a, VFloat b -> VFloat (float_of_int a +. b)
          | VFloat a, VInt b -> VFloat (a +. float_of_int b)
          | VString a, VString b -> VString (a ^ b)
          | VList a, VList b -> VList (a @ b)
          | _ -> failwith "wrong types");
    add_bin_operator program "-" (fun a b ->
        match a, b with
            VInt a, VInt b -> VInt (a - b)
          | VFloat a, VFloat b -> VFloat (a -. b)
          | VInt a, VFloat b -> VFloat (float_of_int a -. b)
          | VFloat a, VInt b -> VFloat (a -. float_of_int b)
          | _ -> failwith "wrong types");
    add_bin_operator program "*" (fun a b ->
        match a, b with
            VInt a, VInt b -> VInt (a * b)
          | VFloat a, VFloat b -> VFloat (a *. b)
          | VInt a, VFloat b -> VFloat (float_of_int a *. b)
          | VFloat a, VInt b -> VFloat (a *. float_of_int b)
          | _ -> failwith "wrong types");
    add_bin_operator program "/" (fun a b ->
        match a, b with
            VInt a, VInt b when b <> 0 -> VInt (a / b)
          | VFloat a, VFloat b -> VFloat (a /. b)
          | VInt a, VFloat b -> VFloat (float_of_int a /. b)
          | VFloat a, VInt b -> VFloat (a /. float_of_int b)
          | _ -> failwith "wrong types");
    add_bin_operator program "%" (fun a b ->
        match a, b with
            VInt a, VInt b when b <> 0 -> VInt (a mod b)
          | _ -> failwith "wrong types");
    add_bin_operator program "**" (fun a b ->
        match a, b with
            VInt a, VInt b -> if b >= 0 then VInt (int_of_float (float_of_int a ** float_of_int b))
                              else VFloat (float_of_int a ** float_of_int b)
          | VFloat a, VFloat b -> VFloat (a ** b)
          | VInt a, VFloat b -> VFloat (float_of_int a ** b)
          | VFloat a, VInt b -> VFloat (a ** float_of_int b)
          | _ -> failwith "wrong types");
    add_bin_operator program ">" (fun a b -> 
        match a, b with
            VInt a, VFloat b -> VBool((float_of_int a) > b)
          | VFloat a, VInt b -> VBool(a > (float_of_int b))
          | _ -> VBool(a > b));
    add_bin_operator program "<" (fun a b -> 
        match a, b with
            VInt a, VFloat b -> VBool((float_of_int a) < b)
          | VFloat a, VInt b -> VBool(a < (float_of_int b))
          | _ -> VBool(a < b));
    add_bin_operator program ">=" (fun a b -> 
        match a, b with
            VInt a, VFloat b -> VBool((float_of_int a) >= b)
          | VFloat a, VInt b -> VBool(a >= (float_of_int b))
          | _ -> VBool(a >= b));
    add_bin_operator program "<=" (fun a b -> 
        match a, b with
            VInt a, VFloat b -> VBool((float_of_int a) <= b)
          | VFloat a, VInt b -> VBool(a <= (float_of_int b))
          | _ -> VBool(a <= b));
    add_bin_operator program "==" (fun a b -> 
        match a, b with
            VInt a, VFloat b -> VBool((float_of_int a) = b)
          | VFloat a, VInt b -> VBool(a = (float_of_int b))
          | _ -> VBool(a = b));
    add_bin_operator program "!=" (fun a b -> 
        match a, b with
            VInt a, VFloat b -> VBool((float_of_int a) <> b)
          | VFloat a, VInt b -> VBool(a <> (float_of_int b))
          | _ -> VBool(a <> b));
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
    add_bin_operator program ":" (let rec go a b =
        let explode s =
            let rec exp i l =
                if i < 0 then l else exp (i - 1) (s.[i] :: l) in
            exp (String.length s - 1) []
        and implode l =
            let res = String.create (List.length l) in
            let rec imp i = function
            | [] -> res
            | c :: l -> res.[i] <- c; imp (i + 1) l in
            imp 0 l |> Bytes.to_string
            in
        match a, b with
            _, VType TAny -> a
          | _, _ when typeof a = b -> a
          | VInt x, VType TFloat -> VFloat (float_of_int x)
          | VInt x, VType TString -> VString (string_of_int x)
          | VInt x, VType TBool -> VBool (x <> 0)
          | VInt x, VType TChar -> VChar (char_of_int x)
          | VFloat x, VType TInt -> VInt (int_of_float x)
          | VFloat x, VType TString -> VString (string_of_float x)
          | VFloat x, VType TBool -> VBool (x <> 0.0)
          | VChar x, VType TString -> VString (String.make 1 x)
          | VChar x, VType TInt -> VInt (int_of_char x)
          | VString x, VType TInt -> VInt (int_of_string x)
          | VString x, VType TFloat -> VFloat (float_of_string x)
          | VString x, VType TBool -> VBool (bool_of_string x)
          | VString x, VType TList -> VList (explode x |> List.map (fun x -> VChar x))
          | VBool x, VType TInt -> VInt (if x then 1 else 0)
          | VBool x, VType TString -> VString (string_of_bool x)
          | VList x, VType TString -> VString (List.map (fun x -> let VChar y = go x (VType TChar) in y) x |> implode)
          | VList a, VTuple b when List.length a = List.length b -> VTuple (List.map2 go a b)
          | VTuple x, VType TList -> VList x
          | VTuple a, VTuple b when List.length a = List.length b -> VTuple (List.map2 go a b)
          | _ -> failwith "wrong types"
          in go );
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
          | _ -> failwith "wrong types");
    add_value program "typeof" (VNative typeof)

let create () = let program = { ids = Hashtbl.create 50;
                    values = Vector.create (Stack.create ());
                    bin_ops = Vector.create (fun _ _ -> VUnit);
                    un_ops = Vector.create (fun _ -> VUnit) }
                in init program; program