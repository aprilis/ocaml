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
and definition = pattern * expression
and definition_list = definition list
and pattern = expression

type statement = Definition of definition
                | DefinitionList of definition_list
                | Expression of expression
                | Import of string
                | Quit
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