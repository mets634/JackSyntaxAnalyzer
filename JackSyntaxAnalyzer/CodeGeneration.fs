module CodeGeneration

open System

open SymbolMap
open ParserType
open VariableType


//////////////Sets in the scope table fields for the class
let classVarDecHandler(treeNode:parserRecord) =
 
    let kind = treeNode.inner.Head //the var kind (static|field)
    let Vtype = treeNode.inner.Item(1) //the var type
    
    let mutable nameIndex:int = 2 //in case multiple variable were defined
    while not(treeNode.inner.Item(nameIndex).value.Equals ";") do //as long as we dont get to the end of the declartion
        nameIndex <- nameIndex + 1
        addToScope(treeNode.inner.Item(nameIndex).value,parserToType(Vtype),parserToKind(kind)) |> ignore
        nameIndex <- nameIndex + 1



//*functions*

/////*function's variables*
//////////////add all the paramters to the function scope as arg
let parameterListHandler(treeNode:parserRecord) = 
    let mutable counter = 0 //going through the nodes (to add all the paramaters)
    
    while treeNode.inner.Length >= counter do
        addToScope(treeNode.inner.Item(counter + 1).value , parserToType(treeNode.inner.Item(counter)),Argument) |> ignore
        counter <- counter + 3 //next parameter

///////////////add all the variable to the funcion's scope as Var
let varDecHandler(treeNode:parserRecord) = 
    let Vtype = parserToType(treeNode.inner.Item(1)) //the var type
    
    let mutable nameIndex:int = 2 //in case multiple variable were defined
    while not(treeNode.inner.Item(nameIndex).value.Equals ";") do //as long as we dont get to the end of the declartion
        nameIndex <- nameIndex + 1
        addToScope(treeNode.inner.Item(nameIndex).value,Vtype,Var) |> ignore
        nameIndex <- nameIndex + 1

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
      
    [""]
and ifGenerate(treeNode:parserRecord) =      //if statement
    [""]
and whileGenerate(treeNode:parserRecord) =   //while statement
    [""]
and doGenerate(treeNode:parserRecord) =      //do statement
    [""]
and returnGenerate(treeNode:parserRecord) =  //return statement
    [""]


let subroutineBodyGenerate(treeNode:parserRecord) =  
    let mutable varIndex = 2 //these will help me loop for the vars tokens

    while treeNode.inner.Item(varIndex).pType.Equals VarDec do
        varDecHandler(treeNode.inner.Item(varIndex)) |> ignore
        varIndex <- varIndex + 1
       
    statementsGenerate(treeNode.inner.Item(varIndex)) // return the code of the statements

let subroutineDecGenerate(treeNode:parserRecord) = 
    newFunction(treeNode.inner.Item(2).value) |> ignore //restart the scope table

    let mutable VmCode = List.Empty //the generated

    if not(treeNode.inner.Head.value.Equals "function") then //set "this" variable 
        addToScope("this",ClassName,Argument)
        VmCode <- VmCode @ ["push argumnet 0"; "pop pointer 0"] //set "THIS" frame
    
    parameterListHandler(treeNode.inner.Item 4) |> ignore
    VmCode <- VmCode @ subroutineBodyGenerate(treeNode.inner.Item 6)
    ["function " + className + "." + functionName + " " + varCount(Var)] @ VmCode



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

