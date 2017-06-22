module CodeGeneration

open SymbolMap
open ParserType

let classVarDecGenarate(treeNode:parserRecord) =
 
    let kind = treeNode.inner.Head.value //the var kind (static|field)
    let Vtype = treeNode.inner.Item(1).value //the var type
    
    let mutable nameIndex:int = 3
    do 
        addToClass(treeNode.inner.Item(nameIndex),) 


let classGenerate(treeNode:parserRecord) = 
    