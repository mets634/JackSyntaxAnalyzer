module expression

open System

open TokenType
open ParserType
open TokenIndex


let unaryOp (token:tokenRecord) = 
    append |> ignore
    tokenToParserRecord(token)

let op (token:tokenRecord) = 
    append |> ignore
    tokenToParserRecord(token)

    
let rec term (tokens:tokenRecord[]) =
    let curr = tokens.[TokenIndex.currIndex]
    let next = tokens.[TokenIndex.currIndex + 1]
    match curr with
    | t when isIntegerConstant(t)  -> append |> ignore
                                      [{ pType=Term; inner=[tokenToParserRecord(t)]; value=t.value }]
    | t when isStringConstant(t) -> append |> ignore
                                    [{ pType=Term; inner=[tokenToParserRecord(t)]; value=t.value }]
    | t when isKeywordConstant(t) -> append |> ignore
                                     [{ pType=Term; inner=[tokenToParserRecord(t)]; value=t.value }]
    | t when isVarName(t) && next.value = "[" -> append |> ignore
                                                 append |> ignore
                                                 let res = [{ pType=Term; 
                                                 inner=[tokenToParserRecord(curr); tokenToParserRecord(next)] @ expression(tokens) @ [{pType=tokenToParser(curr.eType); inner=List.empty; value="]"}]; 
                                                 value=t.value }]
                                                 append |> ignore // read ']'
                                                 res
    | t when isVarName(t) && not (next.value = "[") -> append |> ignore
                                                       [{ pType=Term; inner=[tokenToParserRecord(t)]; value=t.value }]

    | t when t.value = "(" -> append |> ignore
                              let res = [{ pType=Term; inner=tokenToParserRecord(t)::expression(tokens) @ [{pType=tokenToParser(t.eType); inner=List.empty; value=")"}]; value=t.value }]
                              append |> ignore // read ')'
                              res
    | t when isUnaryOp(t) -> append |> ignore
                             unaryOp(t)::term(tokens)
    | _ -> subroutineCall(tokens)


and expression (tokens:tokenRecord[]) =
    let terms = term(tokens)
    let mutable curr = tokens.[TokenIndex.currIndex]

    let mutable acc = list<parserRecord>.Empty
    while isOp(curr) do
        append |> ignore

        // add new tokens
        acc <- acc @ op(curr)::term(tokens)

        curr <- tokens.[TokenIndex.currIndex] // update curr

    acc // return accumulated items


and subroutineCall(tokens:tokenRecord[]) =
    let curr = tokens.[TokenIndex.currIndex]
    let next = tokens.[TokenIndex.currIndex + 1]
    if next.value = "(" then
        // check if subroutine name exists
        append |> ignore
        append |> ignore
        let res = [{ pType=SubroutineCall; 
        inner=[tokenToParserRecord(curr); tokenToParserRecord(next)] @ expressionList(tokens) @ [{pType=Symbol; inner=List.empty; value=")" }];
        value=curr.value }]
        append |> ignore

        res
    elif next.value = "." then
        // check if is class name or var name
        append |> ignore // name

        append |> ignore // period
        let subName = tokens.[TokenIndex.currIndex]
        append |> ignore // '('
        let res = [{ pType=SubroutineCall; 
        inner=[tokenToParserRecord(curr); tokenToParserRecord(next); tokenToParserRecord(subName); {pType=Symbol; inner=List.empty; value="("}] @ expressionList(tokens) @ [{pType=Symbol; inner=List.empty; value=")" }];
        value=curr.value }]
        append |> ignore // ')'

        res

    else
        failwith "hello"

and expressionList(tokens:tokenRecord[]) =
    try
        let mutable acc = expression(tokens)
        let mutable curr = tokens.[TokenIndex.currIndex]

        while curr.value = "," do
            acc <- acc @ expression(tokens)
            acc <- acc @ [tokenToParserRecord(curr)] // add token

            append |> ignore
            curr <- tokens.[TokenIndex.currIndex]

        acc

    with _ -> []
