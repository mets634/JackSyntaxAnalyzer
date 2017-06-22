module VariableType

type variableKind = Static | Field | Var | Argument

type variableType = Int | Char | Boolean | ClassName

type variableRecord = {name:string ; vType:variableType ; vKind:variableKind }