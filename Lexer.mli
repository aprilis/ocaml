type token = LeftBracket 
            | RightBracket
            | SemiColon
            | Equality
            | Keyword of string
            | Operator of string
            | TInt of int
            | TFloat of float * float
            | TString of string
            | TChar of char
            | Id of string
            | Raw of char list

val tokenize: string -> token list