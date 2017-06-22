module VariableType

open ParserType

type variableClassKind = Static  //single copy of this variable must be kept alive at all time 
                            | Field //keep diffrent copies for each object 
                    
                    
type variableMethodKind = Var //created on subroutine entry,killed on exit
                           | Argument //similar to local

type variableType = Int | Char | Boolean | ClassName


let parserToType(t:parserRecord) = 
    match t.value with
    | "int" -> Int
    | "char" -> Char
    | "boolean" -> Boolean
    | v when t.pType.Equals Identifier -> ClassName

let parserToCKind(t:parserRecord) = 
    match t.value with
    | "static" -> Static
    | "field" -> Field

let parserToMKind(t:parserRecord) = 
    match t.value with
    | "var" -> Var
    | v when t.pType.Equals ParamaterList -> Argument



type variableClassRecord = {name:string ; vType:variableType ; vCKind:variableClassKind ;index:int }

type variableMethodRecord = {name:string ; vType:variableType ; vMKind:variableMethodKind ;index:int }