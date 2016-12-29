#use "interpreter.ml";;
let code = "let f a b = (a * 5 - 2, b + 3, [a+3, b], (a), (), [])"
(*let code = "let x = if a == 5 then fun y -> y else True and z = 2 + 3 * 4 ** 5 + 6"*)
(*let code = "let y = 5 and x = if y then \"true\" else \"false\""*);;
let tok = Token.get_tokens code;;
let parsed = Parser.parse tok;;
let program = Program.create();;
init_pervasives program;;
let bound = Program.bind_ids program parsed;;