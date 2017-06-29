module VariableType

open ParserType

type variableKind = Static  //single copy of this variable must be kept alive at all time 
                            | Field //keep diffrent copies for each object 
                            | Var //created on subroutine entry,killed on exit
                            | Argument //similar to local
                            | This //for ctor
                            | None
                    
                   

type variableType = Int | Char | Boolean | ClassName


let parserToType(t:parserRecord) = 
    match t.value with
    | "int" -> Int
    | "char" -> Char
    | "boolean" -> Boolean
    | v when t.pType.Equals Identifier -> ClassName

let parserToKind(t:parserRecord) = 
    match t.value with
    | "static" -> Static
    | "field" -> Field
    | "var" -> Var
    | _ -> Argument

type variableClassRecord = {name:string ; vType:string ; vCKind:variableKind ;index:int ; cName:string }

type variableMethodRecord = {name:string ; vType:string ; vMKind:variableKind ;index:int ; cName:string }