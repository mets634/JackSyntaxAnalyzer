module SymbolMap

open VariableType

let mutable functionScope = [{name = ""; vType = Int; vMKind = Var; index = 0}]

let mutable classScope = [{name = ""; vType = Int; vCKind = Field; index = 0}]

let mutable statementNumber = 0

let newFunction = 
    functionScope <- []

let newClass = 
    classScope <- []
    functionScope <- []
    statementNumber <- 0

let addToMethod(VName:string , VType:variableType , VMKind:variableMethodKind) = 
    functionScope <- {name = VName; vType = VType; vMKind = VMKind; index = functionScope.Length + 1} :: functionScope

let addToClass(VName:string , VType:variableType , VCKind:variableClassKind) = 
    classScope <- {name = VName; vType = VType; vCKind = VCKind; index = functionScope.Length + 1} :: classScope

//var count
//kind of

let typeOf(VName:string) = 
    classScope |> List.filter(fun record -> record.name.Equals VName) |> List.item(0)

