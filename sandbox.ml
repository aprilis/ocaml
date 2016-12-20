#use "interpreter.ml";;
let code = "let x = 3";;
let tok = Token.get_tokens code;;
let parsed = Program.parse tok;;