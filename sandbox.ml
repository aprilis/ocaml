#use "interpreter.ml";;
(*let code = "let x = if a == 5 then fun y -> y else True and z = 2 + 3 * 4 ** 5 + 6"*)
let code = "let y = 5 and x = if y then \"true\" else \"false\"";;
let tok = Token.get_tokens code;;
let parsed = Program.parse tok;;
let program = Program.create();;
let bound = Program.bind_ids program parsed;;