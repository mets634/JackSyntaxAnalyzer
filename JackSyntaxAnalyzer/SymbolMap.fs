module SymbolMap

open VariableType

let mutable functionScope = [{name = ""; vType = ""; vMKind = Var; index = 0}]
let mutable functionName = ""

let mutable classScope = [{name = ""; vType = ""; vCKind = Field; index = 0}]
let mutable className = ""

let mutable statementNumber = 0
let getNumber() = 
    statementNumber <- statementNumber + 1
    (statementNumber - 1).ToString()

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

let varCount(VK:variableKind) = 
    let mutable counter = 0

    match VK with
    | t when (t.Equals Var||t.Equals Argument) -> for row in functionScope do
                                                     if row.vMKind.Equals VK then
                                                        counter <- counter + 1
                                                  counter
    | t when (t.Equals Static||t.Equals Field) -> for row in classScope do
                                                     if row.vCKind.Equals VK then
                                                        counter <- counter + 1
                                                  counter

let addToScope(VName:string , VType:variableType , VMKind:variableKind) = 
    match VMKind with
    | Argument -> functionScope <- {name = VName; vType = TypeToString VType; vMKind = VMKind; index = varCount(VMKind) + 1} :: functionScope
    | Var -> functionScope <- {name = VName; vType = TypeToString VType; vMKind = VMKind; index = varCount(VMKind) + 1} :: functionScope
    | Field -> classScope <- {name = VName; vType = TypeToString VType; vCKind = VMKind; index = varCount(VMKind) + 1} :: classScope
    | Static -> classScope <- {name = VName; vType = TypeToString VType; vCKind = VMKind; index = varCount(VMKind) + 1} :: classScope

//will return a Field if such variable doesn't exist, in addition the funciton will prefer return a method variable
let kindOf(Name:string) = 
    let mutable kind = Field //set to field at first so i can check if type has changed to methodKind
    for row in classScope do
        if row.name.Equals Name then
            kind <- row.vCKind

    for row in functionScope do
        if row.name.Equals Name then
            kind <- row.vMKind
    
    kind

let typeOf(VName:string) = 
    let mutable t = ""

    if classScope.Length > 0 then
        let record = classScope |> List.filter(fun record -> record.name.Equals VName) |> List.item(0) 
        t <- record.vType

    if functionScope.Length > 0 then
        let record = functionScope |> List.filter(fun record -> record.name.Equals VName) |> List.item(0)
        t <- record.vType

    t

let indexOf(VName:string) = 
    let mutable t = 0

    if classScope.Length > 0 then
        let record = classScope |> List.filter(fun record -> record.name.Equals VName) |> List.item(0) 
        t <- record.index

    if functionScope.Length > 0 then
        let record = functionScope |> List.filter(fun record -> record.name.Equals VName) |> List.item(0)
        t <- record.index

    t