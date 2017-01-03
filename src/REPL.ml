exception Quit
exception FileErr of string

let rec print_value value =
    let open Program in
    let special = [('\n', 'n');('\\', '\\')] in
    let special_string, special_char = ('"', '"')::special, ('\t', 't')::('\'', '\'')::special in
    let print_list l =
        match l with
            [] -> ()
            | h::t -> print_value h; List.iter (fun v -> print_string ", "; print_value v) t
        in
    match value with
        VInt x -> print_int x
        | VFloat x -> print_float x
        | VBool x -> print_string (if x then "true" else "false")
        | VChar x -> print_char '\'';
                    begin try let k = List.assoc x special_char in
                        print_char '\\';
                        print_char k
                    with Not_found -> print_char x
                    end;
                    print_char '\'';
        | VString x -> let pr c =
                            try let k = List.assoc c special_string in
                            print_char '\\';
                            print_char k
                            with Not_found -> print_char c
                        in
                        print_char '"';
                        String.iter pr x;
                        print_char '"'
        | VUnit -> print_string "()"
        | VTuple t -> print_char '('; print_list t; print_char ')'
        | VList t -> print_char '['; print_list t; print_char ']'
        | VNative _
        | VFunction (_, _, _) -> print_string "<function>"
        | VType x -> let types = [(TInt, "int");(TFloat, "float");(TString, "string");(TChar,"char");
            (TBool, "bool");(TUnit, "unit");(TList, "list");(TFunction, "function");(TType, "type")] in
            List.assoc x types |> print_string

let split_on_semicolon l =
    let rec go l p =
        match l with
            [] -> []
            | Token.SemiColon :: t ->
                let t = go t [] in if p = [] then t else p :: t
            | h :: t -> go t (h :: p)
    in go l [] |> List.map List.rev

let has_semicolon l =
    l |> Token.get_tokens |> List.mem Token.SemiColon

let open_file path =
    try 
        let file = open_in path in
        let rec read () =
            try let l = input_line file in l :: read () with End_of_file -> []
        in read ()
    with _ -> raise (FileErr path)

let rec eval_statement program l =
    let backup = Program.backup program in
    try
        let statement = l |> Parser.parse |> Program.bind_ids program in 
        match statement with
            Program.Expression e -> Program.eval program e |> print_value; print_newline ()
            | Program.DefinitionList def -> Program.feed program def |> List.iter (fun (name, value) ->
                    print_string name; print_string " = "; print_value value; print_newline ())
            | Program.Quit -> raise Quit
            | Program.Import path -> open_file path |> eval_code program
            | _ -> ()
    with err ->
        Program.restore program backup;
        raise err

and eval_code program lines =
    try
        List.map (String.split_on_char '\n') lines
        |> List.flatten
        |> List.map Token.get_tokens
        |> List.flatten
        |> split_on_semicolon
        |> List.iter (eval_statement program)
    with Token.Err err -> print_endline ("Lexer error: " ^ err)
        | Parser.Err err -> print_endline ("Parser error: " ^ err)
        | Program.RuntimeErr err -> print_endline ("Runtime error: " ^ err)
        | Program.BindErr err -> print_endline ("Unbound value: " ^ err)
        | Program.InternalErr err -> print_endline ("Internal error: " ^ err)
        | Quit -> raise Quit
        | FileErr err -> print_endline ("Failed to read file: " ^ err)
        (*| _ -> print_endline "Other error"*)

let start () =
    let program = Program.create () in
    try while true do
        print_string "# ";
        let lines = ref [] and finished = ref false in
        while not !finished do
            try let l = read_line () in
                lines := l :: !lines;
                if has_semicolon l then finished := true else print_string "  "
            with End_of_file -> raise Quit
        done;
        eval_code program (List.rev !lines)
    done with Quit -> ()