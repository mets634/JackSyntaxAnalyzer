﻿module CodeGeneration

open System

open SymbolMap
open ParserType
open VariableType
open CodeGeneration2


//////////////Sets in the scope table fields for the class
let classVarDecHandler(treeNode:parserRecord) =
 
    let kind = treeNode.inner.Head //the var kind (static|field)
    let Vtype = treeNode.inner.Item(1) //the var type
    
    let mutable nameIndex:int = 2 //in case multiple variable were defined
    while not(treeNode.inner.Item(nameIndex - 1).value.Equals ";") && not(treeNode.inner.Item(nameIndex).value.Equals ";") do //as long as we dont get to the end of the declartion
        addToScope(treeNode.inner.Item(nameIndex).value,parserToType(Vtype),parserToKind(kind),Vtype.value) |> ignore
        nameIndex <- nameIndex + 2



//*functions*

/////*function's variables*
//////////////add all the paramters to the function scope as arg
let parameterListHandler(treeNode:parserRecord) = 
    let mutable counter = 0 //going through the nodes (to add all the paramaters)
    
    while treeNode.inner.Length > counter do
        addToScope(treeNode.inner.Item(counter + 1).value , parserToType(treeNode.inner.Item(counter)),Argument,treeNode.inner.Item(counter).value) |> ignore
        counter <- counter + 3 //next parameter

///////////////add all the variable to the funcion's scope as Var
let varDecHandler(treeNode:parserRecord) = 
    let Vtype = parserToType(treeNode.inner.Item(1)) //the var type
    
    let mutable nameIndex:int = 2 //in case multiple variable were defined
    while not(treeNode.inner.Item(nameIndex - 1).value.Equals ";") && not(treeNode.inner.Item(nameIndex).value.Equals ";") do //as long as we dont get to the end of the declartion
        addToScope(treeNode.inner.Item(nameIndex).value,Vtype,Var,treeNode.inner.Item(1).value) |> ignore
        nameIndex <- nameIndex + 2

//////*function's code*
//////////////// return the code of all the combined statements
let rec statementsGenerate(treeNode:parserRecord) =
    let mutable VmCode = List.empty //the code we give back
     
    for statement in treeNode.inner do       
        VmCode <- VmCode @ match statement.pType with 
                           | LetStatement -> letGenerate statement
                           | IfStatement -> ifGenerate statement
                           | WhileStatement -> whileGenerate statement
                           | DoStatement -> doGenerate statement
                           | ReturnStatement -> returnGenerate statement
    ///return the combine code
    VmCode         

///leave this to me
and letGenerate(treeNode:parserRecord) =     //let statement  
    let mutable VmCode = List.empty<string>
    
    let stack = getStack(treeNode.inner.Item(1).value)
    
    if treeNode.inner.Item(2).value.Equals("[") then //in case of array
        VmCode <- VmCode @ ["push " + stack + " " + indexOf(treeNode.inner.Item(1).value).ToString()] @ expressionGenerate(treeNode.inner.Item 3) @
                     ["add"] @ expressionGenerate(treeNode.inner.Item(treeNode.inner.Length - 2)) @ ["pop temp 0";"pop pointer 1";"push temp 0";"pop that 0"]
    else 
        VmCode <- VmCode @ expressionGenerate(treeNode.inner.Item(treeNode.inner.Length - 2)) @ ["pop " + stack + " " + indexOf(treeNode.inner.Item(1).value).ToString()]
    VmCode

and ifGenerate(treeNode:parserRecord) =      //if statement
    let mutable VmCode = expressionGenerate(treeNode.inner.Item 2)
    let idx  = getNumber()

    VmCode <- VmCode @ ["if-goto IF_TRUE" + idx; "goto IF_FALSE" + idx;"label IF_TRUE" + idx] @ statementsGenerate(treeNode.inner.Item 5)

    if treeNode.inner.Length > 7 then //in case of 'else statement
        VmCode <- VmCode @ ["goto IF_END" + idx ;"label IF_FALSE" + idx] @ statementsGenerate(treeNode.inner.Item 9) @ ["label IF_END" + idx]
    else
        VmCode <- VmCode @ ["label IF_FALSE" + idx]
    VmCode

and whileGenerate(treeNode:parserRecord) =   //while statement
    let idx = getNumber()
    ["label WHILE_EXP" + idx; ] @ expressionGenerate(treeNode.inner.Item 2) @ ["not";"if-goto WHILE_END" + idx] 
                         @ statementsGenerate(treeNode.inner.Item 5) @ ["goto WHILE_EXP" + idx;"label WHILE_END" + idx]
    
and doGenerate(treeNode:parserRecord) =      //do statement
    let mutable VmCode = List.Empty

    let mutable isMethod = 0
    //push this arg
    if treeNode.inner.Item(2).value.Equals "." then // if its a call with a variable
        if not(kindOf(treeNode.inner.Item(1).value).Equals None) then
            VmCode <- VmCode @ ["push " + getStack(treeNode.inner.Item(1).value) + " " + indexOf(treeNode.inner.Item(1).value).ToString()]
            isMethod <- 1
    else 
        VmCode <- VmCode @ ["push pointer 0"]
        isMethod <- 1

    VmCode <- VmCode @ expressionListGenerate(treeNode.inner.Item(treeNode.inner.Length - 3)) //push all the parameters to the list
    let paramList =  treeNode.inner |> List.find (fun p -> p.pType = ExpressionList)
    let mutable paramsNumber = 0
    if not(paramList.inner.Length.Equals 0) then
        paramsNumber <-  paramList.inner.Length / 2 + 1
        
    paramsNumber <- paramsNumber + isMethod //in case we added "this" arg

    if treeNode.inner.Item(2).value.Equals "." then // if its a call with a variable
        if not(kindOf(treeNode.inner.Item(1).value).Equals None) then
            VmCode <- VmCode @ ["call " + varClassName(treeNode.inner.Item(1).value) + treeNode.inner.Item(2).value + treeNode.inner.Item(3).value + " " + paramsNumber.ToString()]
        else
            VmCode <- VmCode @ ["call " + treeNode.inner.Item(1).value + treeNode.inner.Item(2).value + treeNode.inner.Item(3).value + " " + paramsNumber.ToString()]       
    else
        VmCode <- VmCode @ ["call " + className + "." + treeNode.inner.Item(1).value + " " + paramsNumber.ToString()]    
    VmCode @ ["pop temp 0"]

and returnGenerate(treeNode:parserRecord) =  //return statement
    let mutable VmCode = ["return"]

    if treeNode.inner.Length > 2 then //in case i should return a vlaue
        VmCode <- expressionGenerate(treeNode.inner.Item 1) @ VmCode
    else 
        VmCode <- ["push constant 0"] @ VmCode
    VmCode

let subroutineBodyGenerate(treeNode:parserRecord) =  
    let mutable varIndex = 1 //these will help me loop for the vars tokens

    while treeNode.inner.Item(varIndex).pType.Equals VarDec do
        varDecHandler(treeNode.inner.Item(varIndex)) |> ignore
        varIndex <- varIndex + 1
       
    statementsGenerate(treeNode.inner.Item(varIndex)) // return the code of the statements

let subroutineDecGenerate(treeNode:parserRecord) = 
    newFunction(treeNode.inner.Item(2).value) |> ignore //restart the scope table

    let mutable VmCode = List.Empty //the generated

    if treeNode.inner.Head.value.Equals "method" then //set "this" variable 
        addToScope("this",ClassName,Argument,className)
        VmCode <- VmCode @ ["push argument 0"; "pop pointer 0"] //set "THIS" frame
    if treeNode.inner.Head.value.Equals "constructor" then //set "this" variable 
        addToScope("this",ClassName,This,className)
        VmCode <- VmCode @ ["push constant " + varCount(Field).ToString(); "call Memory.alloc 1";"pop pointer 0"] //set "THIS" frame
        
    
    parameterListHandler(treeNode.inner.Item 4) |> ignore
    VmCode <- VmCode @ subroutineBodyGenerate(treeNode.inner.Item 6)
    ["function " + className + "." + functionName + " " + varCount(Var).ToString()] @ VmCode



let classGenerate(treeNode:parserRecord) = 
    newClass(treeNode.inner.Item(1).value) |> ignore
    
    let mutable VmCode = List.Empty

    let mutable index = 3
    while treeNode.inner.Item(index).pType.Equals ClassVarDec do //add all the class fields
        classVarDecHandler(treeNode.inner.Item(index)) |> ignore
        index <- index + 1

    while treeNode.inner.Item(index).pType.Equals SubroutineDec do // add all the class subroutines
        VmCode <- VmCode @ subroutineDecGenerate(treeNode.inner.Item(index))
        index <- index + 1

    VmCode

