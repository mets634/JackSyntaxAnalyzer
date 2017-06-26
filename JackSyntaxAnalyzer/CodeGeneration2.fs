module CodeGeneration2

open System
open System.Text

open SymbolMap
open ParserType
open VariableType


let keywordConstantGenerate (p:parserRecord) = 
    match p.value with
    | "true" -> ["push constant -1"]
    | "false" -> ["push constant 0"]
    | "null" -> [] // MAY NEED TO MODIFY
    | "this" -> ["push pointer 0"]

let unaryOpGenerate (p:parserRecord) = 
    match p.value with
    | @"-" -> ["neg"]
    | @"~" -> ["not"]

let opGenerate (p:parserRecord) =
    match p.value with
    | @"+" -> ["add"]
    | @"-" -> ["sub"]
    | @"*" -> ["call Math.multiply 2"]
    | @"/" -> ["call Math.divide"]
    | @"&" -> ["and"]
    | @"|" -> ["or"]
    | @"<" -> ["lt"]
    | @">" -> ["gt"]
    | @"=" -> ["eq"]

let rec termGenerate (p:parserRecord) = 
    let stringGenerate (r:parserRecord) = 
        String.Format("push constant {0}", r.value.Length) :: 
        [for c in Encoding.ASCII.GetBytes(r.value) ->  
         "push constant " + c.ToString() + Environment.NewLine + "call String.appendChar 2"]
    
    let subroutineCallGenerate (p:parserRecord) =
        let paramList =  p.inner |> List.find (fun p -> p.pType = ExpressionList)
        let paramsNumber =  (paramList.inner.Length + 1) / 2
        if p.inner.[1].value = "." then // class-scoped function
            if isVar(p.inner.[0].value) then // is variable, need to push instance
                "push " + getStack(p.inner.[0].value) + " " + indexOf(p.inner.[0].value).ToString() :: 
                expressionListGenerate(p.inner.[4]) @
                 ["call " + p.inner.[0].value + "." + p.inner.[2].value + " " + (paramsNumber + 1).ToString()]
            else // static method
                expressionListGenerate(p.inner.[4]) @ ["call " + p.inner.[0].value + "." + p.inner.[2].value + " " + paramsNumber.ToString()]
        else // regular function
            expressionListGenerate(p.inner.[2]) @ ["call " + p.inner.[0].value + " " + paramsNumber.ToString()]
    
    let inner1 = p.inner.[0] // first item

    if p.inner.Length = 1 then
        match inner1.pType with
        | parserType.IntegerConstant -> ["push constant " + p.value]
        | parserType.KeyWord -> keywordConstantGenerate p
        | parserType.StringConstant -> stringGenerate inner1
        | parserType.Identifier -> ["push " + getStack(inner1.value) + " " + indexOf(inner1.value).ToString()] // MAY NEEDS CHECKING
    else 
        match p.inner with
        | r when r.[0].value = "(" -> expressionGenerate(r.[1])
        | r when r.[1].value =  "[" -> "push " + getStack(inner1.value) + " " + indexOf(inner1.value).ToString() :: expressionGenerate(r.[2]) @ ["add"] // MAY NEED CHECKING                           
        | r when List.exists (fun t -> t.pType = ExpressionList) r -> subroutineCallGenerate(p)
        | r -> termGenerate(p.inner.[1]) @ unaryOpGenerate(p.inner.[0])

and expressionGenerate (p:parserRecord) = 
    if p.value = ";" then 
        List<string>.Empty
    else

        let mutable counter = 1
        let mutable acc = termGenerate(p.inner.[0])

        while counter < p.inner.Length do // while more 'op term'
            acc <- acc @ termGenerate(p.inner.[counter + 1]) @ opGenerate(p.inner.[counter])
            counter <- counter + 2

        acc

and expressionListGenerate (p:parserRecord) = 
    if p.inner.Length = 0 then
        [] // no code needed. empty expression
    else
        let mutable counter = 2
        let mutable acc = expressionGenerate(p.inner.[0])

        while counter < p.inner.Length do // while more ', expression'
            acc <- acc @ expressionGenerate(p.inner.[counter])
            counter <- counter + 2

        acc