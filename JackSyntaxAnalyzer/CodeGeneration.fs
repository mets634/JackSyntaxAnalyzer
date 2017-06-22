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
        addToClass(treeNode.inner.Item(nameIndex).value,parserToType(Vtype),parserToCKind(kind)) |> ignore
        nameIndex <- nameIndex + 1

///////////////


//////////////add all the paramters to the function scope as arg

let parameterListHandler(treeNode:parserRecord) = 
    let mutable counter = 0 //going through the nodes (to add all the paramaters)
    
    while treeNode.inner.Length >= counter do
        addToMethod(treeNode.inner.Item(counter + 1).value , parserToType(treeNode.inner.Item(counter)),Argument) |> ignore
        counter <- counter + 3 //next parameter

///////////////


///////////////add all the variable to the funcion's scope as Var

let subVarDecHandler(treeNode:parserRecord) = 
    let Vtype = parserToType(treeNode.inner.Item(1)) //the var type
    
    let mutable nameIndex:int = 2 //in case multiple variable were defined
    while not(treeNode.inner.Item(nameIndex).value.Equals ";") do //as long as we dont get to the end of the declartion
        nameIndex <- nameIndex + 1
        addToMethod(treeNode.inner.Item(nameIndex).value,Vtype,Var) |> ignore
        nameIndex <- nameIndex + 1

////////////////


//////////////// return the code of all the combined statements

let rec statementsGenerate(treeNode:parserRecord) =
    let mutable VmCode = "" //the code we give back
     
    for statement in treeNode.inner do
        
        VmCode <- VmCode + match statement.pType with 
                           | LetStatement -> letGenerate statement
                           | IfStatement -> ifGenerate statement
                           | WhileStatement -> whileGenerate statement
                           | DoStatement -> doGenerate statement
                           | ReturnStatement -> returnGenerate statement
    ///return the combine code
    VmCode         

and letGenerate(treeNode:parserRecord) =     //let statement    
    ""
and ifGenerate(treeNode:parserRecord) =      //if statement
    ""
and whileGenerate(treeNode:parserRecord) =   //while statement
    ""
and doGenerate(treeNode:parserRecord) =      //do statement
    ""
and returnGenerate(treeNode:parserRecord) =  //return statement
    ""

let subroutineDecGenerate(treeNode:parserRecord) = 
    match treeNode.inner.Head.value with
    | "function" -> 

let classGenerate(treeNode:parserRecord) = 
    newClass() |> ignore
