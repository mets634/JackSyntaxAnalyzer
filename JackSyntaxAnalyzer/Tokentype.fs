module TokenType

type elementType = KeyWord | Symbol | IntegerConstant | StringConstant | Identifier
type tokenRecord = { eType:elementType; value:string }