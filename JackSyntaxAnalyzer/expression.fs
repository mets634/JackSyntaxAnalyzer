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

let rec statements(tokens:tokenRecord[]) = 
    let mutable r = statement(tokens)
    let mutable curr = tokens.[TokenIndex.currIndex]

    //while there are still statements
    while curr.value = "let" || curr.value = "if" || curr.value = "while" || curr.value = "do" || curr.value = "return" do
        r <- r @ statement(tokens)
        curr <- tokens.[TokenIndex.currIndex]
    r

and statement(tokens:tokenRecord[]) =
    let curr = tokens.[TokenIndex.currIndex]

    match curr.value with
    | "let" -> letStatement(tokens)
    | "if" -> ifStatement(tokens)
    | "while" -> whileStatement(tokens)
    | "do" -> doStatement(tokens)
    | "return" -> returnStatment(tokens)

and returnStatment(tokens:tokenRecord[]) = 
    let curr = tokens.[TokenIndex.currIndex]
    append |> ignore
    let next = tokens.[TokenIndex.currIndex]
    let mutable r = []
  
    //now check if the code give back an expression
    if next.value = ";" then 
        r <- [tokenToParserRecord(curr)] @ [tokenToParserRecord(next)]
    else 
        r <- [tokenToParserRecord(curr)] @ expression(tokens)
        let fianl = tokens.[TokenIndex.currIndex]
        append |> ignore
        r <- r @ [tokenToParserRecord(fianl)]
    [{pType = ReturnStatement; inner = r; value = "returnStatement"}]

 and doStatement(tokens:tokenRecord[]) =
    let curr = tokens.[TokenIndex.currIndex] //'do' token
    append |> ignore
    let subCall = subroutineCall(tokens)
    let fianl = tokens.[TokenIndex.currIndex] // ';' token
    append |> ignore

    [{pType = DoStatement; inner = [tokenToParserRecord(curr)] @ subCall @ [tokenToParserRecord(fianl)]; value = ""}]

and whileStatement(tokens:tokenRecord[]) =
    //the while with (expression)
    let curr = tokens.[TokenIndex.currIndex]
    append |> ignore
    let openB = tokens.[TokenIndex.currIndex]
    append |> ignore
    let expre = expression(tokens)
    let closeB = tokens.[TokenIndex.currIndex]
    append |> ignore

    //the statements (with {})
    let openS = tokens.[TokenIndex.currIndex]
    append |> ignore
    let Statements = statements(tokens)
    let closeS = tokens.[TokenIndex.currIndex]
    append |> ignore

    let r = [tokenToParserRecord(curr)] @ [tokenToParserRecord(openB)] @ expre @ [tokenToParserRecord(closeB)] @ [tokenToParserRecord(openS)] @ Statements @ [tokenToParserRecord(closeS)]
    [{pType = WhileStatement; inner = r; value = ""}]

and ifStatement(tokens:tokenRecord[]) =
   //the while with (expression)
    let curr = tokens.[TokenIndex.currIndex]
    append |> ignore
    let openB = tokens.[TokenIndex.currIndex]
    append |> ignore
    let expre = expression(tokens)
    let closeB = tokens.[TokenIndex.currIndex]
    append |> ignore

    //the statements (with {})
    let openS = tokens.[TokenIndex.currIndex]
    append |> ignore
    let Statements = statements(tokens)
    let closeS = tokens.[TokenIndex.currIndex]
    append |> ignore

    //the statement so far
    let mutable r = [tokenToParserRecord(curr)] @ [tokenToParserRecord(openB)] @ expre @ [tokenToParserRecord(closeB)] @ [tokenToParserRecord(openS)] @ Statements @ [tokenToParserRecord(closeS)]
    
    //check if there is else 
    let elseS = tokens.[TokenIndex.currIndex]
    if(elseS.value = "else") then
        append |> ignore
        let elseOpenS = tokens.[TokenIndex.currIndex]
        append |> ignore
        let elseStatements = statements(tokens)
        let elseCloseS = tokens.[TokenIndex.currIndex]
        append |> ignore

        r <- r @ [tokenToParserRecord(elseS)] @ [tokenToParserRecord(elseOpenS)] @ elseStatements @ [tokenToParserRecord(elseCloseS)]
     
    [{pType = IfStatement; inner = r; value = ""}]

and letStatement(tokens:tokenRecord[]) =
    let letS = tokens.[TokenIndex.currIndex]
    append |> ignore
    let varName = tokens.[TokenIndex.currIndex]
    append |> ignore
    
    let next = tokens.[TokenIndex.currIndex]
    append |> ignore

    let mutable r = [tokenToParserRecord(letS)] @ [tokenToParserRecord(varName)] @ [tokenToParserRecord(next)]

    //check if there is also ('['expression']') part
    if(next.value = "[") then
        let expre = expression(tokens)
        let closeB = tokens.[TokenIndex.currIndex]
        append |> ignore
        let equal = tokens.[TokenIndex.currIndex]
        append |> ignore

        r <- r @ expre @ [tokenToParserRecord(closeB)] @ [tokenToParserRecord(equal)]

    //the part after the '='
    let expreS = expression(tokens)
    let fianl = tokens.[TokenIndex.currIndex]
    append |> ignore
    
    [{pType = LetStatement; inner = r @ expreS @ [tokenToParserRecord(fianl)]; value = ""}]
    



let varName(tokens:tokenRecord[]) =
    let curr = tokens.[TokenIndex.currIndex]
    append |> ignore

    [tokenToParserRecord(curr)]

let subroutineName(tokens:tokenRecord[]) =
    let curr = tokens.[TokenIndex.currIndex]
    append |> ignore

    [tokenToParserRecord(curr)]

let className(tokens:tokenRecord[]) =
    let curr = tokens.[TokenIndex.currIndex]
    append |> ignore

    [tokenToParserRecord(curr)]

let typeStructure(tokens:tokenRecord[]) =
    let curr = tokens.[TokenIndex.currIndex]
    append |> ignore
    [tokenToParserRecord(curr)]

let varDec(tokens:tokenRecord[]) =
    let var = tokens.[TokenIndex.currIndex]
    append |> ignore
    
    let mutable r = []
    r <- [tokenToParserRecord(var)] @ typeStructure(tokens) @ varName(tokens)

    let mutable curr = tokens.[TokenIndex.currIndex]
    //going over all the vars declrations
    while curr.value = "," do
        append |> ignore
        r <- r @ [tokenToParserRecord(curr)] @ varName(tokens)
        curr <- tokens.[TokenIndex.currIndex]
        
    r

let subroutineBody(tokens:tokenRecord[]) =
    let openB = tokens.[TokenIndex.currIndex]
    append |> ignore

    let mutable r = []

    while tokens.[TokenIndex.currIndex].value = "var" do
        r <- r @ varDec(tokens)

    let state = statements(tokens)
    let closeB = tokens.[TokenIndex.currIndex]
    append |> ignore

    [tokenToParserRecord(openB)] @ r @ state @ [tokenToParserRecord(closeB)]

let paramaterList(tokens:tokenRecord[]) =
    let mutable curr = tokens.[TokenIndex.currIndex]
    let mutable r = []

    //check if there are any parameters
    if isType(curr) then
        r <- typeStructure(tokens) @ varName(tokens)
        curr <- tokens.[TokenIndex.currIndex]
        while curr.value = "," do
            append |> ignore
            r <- r @ typeStructure(tokens) @ varName(tokens)
            curr <- tokens.[TokenIndex.currIndex]

    r
   
let subroutineDec(tokens:tokenRecord[]) =
    let declartion = tokens.[TokenIndex.currIndex]
    append |> ignore
    let mutable r = []

    match declartion.value with
    | "constructor" -> r <- [tokenToParserRecord(declartion)] 
    | "function" -> r <- [tokenToParserRecord(declartion)]
    | "method" -> r <- [tokenToParserRecord(declartion)]

    let kind = tokens.[TokenIndex.currIndex]

    if kind.value = "void" then
        r <- r @ [tokenToParserRecord(kind)]
        append |> ignore
    if isType(kind) then
        r <- r @ typeStructure(tokens)
     
    r <- r @ subroutineName(tokens)

    let openS = tokens.[TokenIndex.currIndex]
    append |> ignore
    let PL = paramaterList(tokens)
    let closeS = tokens.[TokenIndex.currIndex]
    append |> ignore
    
    r @ [tokenToParserRecord(openS)] @ PL @ [tokenToParserRecord(closeS)] @ subroutineBody(tokens)

let classVarDec(tokens:tokenRecord[]) =
    let mutable r = []
    let kind = tokens.[TokenIndex.currIndex] // static|field
    append |> ignore

    r <- [tokenToParserRecord(kind)] @ typeStructure(tokens) @ varName(tokens)

    let mutable curr = tokens.[TokenIndex.currIndex] 
    while curr.value = "," do
        append |> ignore
        r <- r @ [tokenToParserRecord(curr)] @ varName(tokens)
        curr <- tokens.[TokenIndex.currIndex]

    let final = tokens.[TokenIndex.currIndex] //the ';'
    append |> ignore

    r @ [tokenToParserRecord(final)]

let classStructure(tokens:tokenRecord[]) =
    let mutable r = []
    let s = tokens.[TokenIndex.currIndex] //the 'class' token
    append |> ignore

    r <- [tokenToParserRecord(s)] @ className(tokens) @[tokenToParserRecord(tokens.[TokenIndex.currIndex])] //including the '{'
    append |> ignore

    while isClassVarDec(tokens.[TokenIndex.currIndex]) do
        r <- r @ classVarDec(tokens)

    while isSubroutineDec(tokens.[TokenIndex.currIndex]) do
        r <- r @ subroutineDec(tokens)

    let fianl = tokens.[TokenIndex.currIndex]
    append |> ignore

    r @ [tokenToParserRecord(fianl)]
