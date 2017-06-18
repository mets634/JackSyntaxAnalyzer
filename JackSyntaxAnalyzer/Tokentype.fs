module TokenType

type elementType = KeyWord | Symbol | IntegerConstant | StringConstant | Identifier | IgnoreMe
let elementTypeToString (e:elementType) =
    match e with
    | KeyWord -> "keyword"
    | Symbol -> "symbol"
    | IntegerConstant -> "integerConstant"
    | StringConstant -> "stringConstant"
    | Identifier -> "identifier"
    | _ -> ""

type tokenRecord = { eType:elementType; value:string }
exception TokenException of string 


