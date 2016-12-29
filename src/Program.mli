type varID = TextID of string | IndexID of int * string
type value = VInt of int 
            | VFloat of float 
            | VString of string 
            | VChar of char
            | VBool of bool
            | VUnit
            | VList of value list 
            | VTuple of value list
            | VFunction of pattern * expression
            | VUnOperator of (value -> value)
            | VBinOperator of (value -> value -> value)
and expression = Constant of value
                | Variable of varID
                | Call of expression * expression
                | Lambda of pattern * expression
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

type t = { ids: (string, int) Hashtbl.t; values: value option Stack.t Vector.t }

val create: unit -> t
val parse: Token.t list -> statement
val bind_ids: t -> statement -> statement