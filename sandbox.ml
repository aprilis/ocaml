#use "interpreter.ml";;
let code = "let x = if a == 5 then fun y -> y else True and z = 2 + 3 * 4 ** 5 + 6";;
let tok = Token.get_tokens code;;
let parsed = Program.parse tok;;