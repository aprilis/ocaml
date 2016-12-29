type t = LeftBracket 
        | RightBracket
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

val get_tokens: string -> t list