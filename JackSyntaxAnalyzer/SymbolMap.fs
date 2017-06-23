module SymbolMap

open VariableType

let mutable functionScope = [{name = ""; vType = ""; vMKind = Var; index = 0}]
let mutable functionName = ""

let mutable classScope = [{name = ""; vType = ""; vCKind = Field; index = 0}]
let mutable className = ""

let mutable statementNumber = 0

let newFunction(funcName:string) = 
    functionScope <- []
    functionName <- funcName


let newClass(cName:string) = 
    classScope <- []
    functionScope <- []
    statementNumber <- 0
    className <- cName

let TypeToString(vT:variableType) = 
    match vT with
    | Int -> "int"
    | Char -> "char"
    | Boolean -> "boolean"
    | ClassName -> className

let addToMethod(VName:string , VType:variableType , VMKind:variableMethodKind) = 
    functionScope <- {name = VName; vType = TypeToString VType; vMKind = VMKind; index = functionScope.Length + 1} :: functionScope

let addToClass(VName:string , VType:variableType , VCKind:variableClassKind) = 
    classScope <- {name = VName; vType = TypeToString VType; vCKind = VCKind; index = functionScope.Length + 1} :: classScope

let methodVarCount(VK:variableMethodKind) = 
    let mutable counter = 0
    
    for row in functionScope do
        if row.vMKind.Equals VK then
            counter <- counter + 1
    counter.ToString()

let classVarCount(VK:variableClassKind) = 
    let mutable counter = 0
    
    for row in classScope do
        if row.vCKind.Equals VK then
            counter <- counter + 1
    counter.ToString()

//kind of

let typeOf(VName:string) = 
    classScope |> List.filter(fun record -> record.name.Equals VName) |> List.item(0)
