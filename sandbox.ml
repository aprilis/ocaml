#use "interpreter.ml";;
let code = "let map f l =
                if l == []
                    then []
                else
                    let h::t = l in
                    f h :: map f t;
            map (fun x -> x * x) [1, 2.4, -3];"
(*let code = "let rev l = let go l r = if l == [] then r else let h::t = l in go t (h::r) in go l [] in rev [1,2,3]"*)
(*let code = "let map f l = if l == [] then [] else let h::t = l in f h :: map f t in let f = map fun x -> x * x in f [1, 2, 3.5]"*)
(*let code = "let x = 10 and y = x * x in if y >= 100 then \"ok\" else \"not ok\""*)
(*let code = "let f a b c = a + b * c"*)
(*let code = "let f a b = (a * 5 - 2, b + 3, [a+3, b], (a), (), [], true)"*)
(*let code = "let x = if x == 5 then fun h::t -> (h + x)::t else true and z = 2 + 3 * 4 ** 5 + 6"*)
(*let code = "let y = 5 and x = if y then \"true\" else \"false\""*);;
let tok = Token.get_tokens code;;
let parsed = Parser.parse tok;;
let program = Program.create();;
init_operators program;;
REPL.eval_code program code;;
let bound = Program.bind_ids program parsed;;
try let Program.Expression e = bound in Program.eval program e with _ -> Program.VUnit;;
try let Program.DefinitionList def = bound in Program.feed program def with _ -> [];;