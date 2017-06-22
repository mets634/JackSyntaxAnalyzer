module VariableType

open ParserType

type variableClassKind = Static  //single copy of this variable must be kept alive at all time 
                            | Field //keep diffrent copies for each object 
                    
                    
type variableMethodKind = Var //created on subroutine entry,killed on exit
                           | Argument //similar to local

type variableType = Int | Char | Boolean | ClassName


let stringToType(t:string) = 
    match t with
    | "int" -> Int
    | "char" -> Char
    | "boolean" -> Boolean
    | t when 



type variableClassRecord = {name:string ; vType:variableType ; vCKind:variableClassKind ;index:int }

type variableMethodRecord = {name:string ; vType:variableType ; vMKind:variableMethodKind ;index:int }