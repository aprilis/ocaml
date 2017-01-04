type t = LeftBracket 
        | RightBracket
        | LeftSBracket
        | RightSBracket
        | Comma
        | SemiColon
        | Equality
        | Arrow
        | DArrow
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

exception Err of string

val letters: string
val get_tokens: string -> t list