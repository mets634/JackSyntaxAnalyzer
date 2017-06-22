module expression

open System

open TokenType
open ParserType
open TokenIndex

let mutable index = 0


let unaryOp (token:tokenRecord) = 
    index <- index + 1
    tokenToParserRecord(token)

let op (token:tokenRecord) = 
    index <- index + 1
    [tokenToParserRecord(token)]

let subroutineName(tokens:tokenRecord[]) =
    let curr = tokens.[index]
    index <- index + 1

    [tokenToParserRecord(curr)]

    
let rec term (tokens:tokenRecord[]) =
    let curr = tokens.[index]
    let next = tokens.[index + 1]
    match curr with
    | t when isIntegerConstant(t)  -> index <- index + 1
                                      [{ pType=Term; inner=[tokenToParserRecord(t)]; value=t.value }]
    | t when isStringConstant(t) -> index <- index + 1
                                    [{ pType=Term; inner=[tokenToParserRecord(t)]; value=t.value }]
    | t when isKeywordConstant(t) -> index <- index + 1
                                     [{ pType=Term; inner=[tokenToParserRecord(t)]; value=t.value }]
    | t when isVarName(t) && next.value = "[" -> index <- index + 1
                                                 index <- index + 1
                                                 let res = [{ pType=Term; 
                                                 inner=[tokenToParserRecord(curr); tokenToParserRecord(next)] @ expression(tokens) @ [{pType=tokenToParser(elementType.Symbol); inner=List.empty; value="]"}]; 
                                                 value=t.value }]
                                                 index <- index + 1 // read ']'
                                                 res
    | t when isVarName(t) && not (next.value = "[" || next.value = ".") -> index <- index + 1
                                                                           [{ pType=Term; inner=[tokenToParserRecord(t)]; value=t.value }]
    | t when t.value = "(" -> index <- index + 1
                              let res = [{ pType=Term; inner=tokenToParserRecord(t)::expression(tokens) @ [{pType=tokenToParser(t.eType); inner=List.empty; value=")"}]; value=t.value }]
                              index <- index + 1 // read ')'
                              res
    | t when isUnaryOp(t) -> [{pType = Term; inner = unaryOp(t)::term(tokens); value = ""}]
    | _ -> [{pType = Term; inner = subroutineCall(tokens); value = ""}]

and expression (tokens:tokenRecord[]) =
    let mutable r = term(tokens)
    let mutable curr = tokens.[index]

    while isOp(curr) do
        r <- r @ op(curr) @ term(tokens)

        curr <- tokens.[index] //upate curr

    [{pType = Expression; inner = r; value = ""}]

and subroutineCall(tokens:tokenRecord[]) =
    let curr = tokens.[index]
    let next = tokens.[index + 1]
    if next.value = "(" then
        // check if subroutine name exists
        index <- index + 1
        index <- index + 1
        let res = [tokenToParserRecord(curr); tokenToParserRecord(next)] @ expressionList(tokens) @ [{pType=Symbol; inner=List.empty; value=")" }]
        index <- index + 1

        res
    elif next.value = "." then
        // check if is class name or var name
        index <- index + 1 // name

        index <- index + 1 // period
        let subName = tokens.[index]
        index <- index + 1 // 'subName'
        index <- index + 1 // '('
        let r =[tokenToParserRecord(curr); tokenToParserRecord(next); tokenToParserRecord(subName); {pType=Symbol; inner=List.empty; value="("}] @ expressionList(tokens) @ [{pType=Symbol; inner=List.empty; value=")" }]
        index <- index + 1 // ')'

        r

    else
        failwith "hello"

and expressionList(tokens:tokenRecord[]) =
    try
        let mutable acc = expression(tokens)
        let mutable curr = tokens.[index]

        while curr.value = "," do
            index <- index + 1
            let temp = expression(tokens)
            acc <- acc @ [tokenToParserRecord(curr)] @ temp

            curr <- tokens.[index]

        [{pType = ExpressionList; inner = acc; value = ""}]

    with _ -> [{pType = ExpressionList; inner = List.empty; value = ""}]

let rec statements(tokens:tokenRecord[]) = 
    let mutable r = statement(tokens)
    let mutable curr = tokens.[index]

    //while there are still statements
    while curr.value = "let" || curr.value = "if" || curr.value = "while" || curr.value = "do" || curr.value = "return" do
        r <- r @ statement(tokens)
        curr <- tokens.[index]
    [{pType = Statements; inner = r; value = ""}]

and statement(tokens:tokenRecord[]) =
    let curr = tokens.[index]

    match curr.value with
    | "let" -> letStatement(tokens)
    | "if" -> ifStatement(tokens)
    | "while" -> whileStatement(tokens)
    | "do" -> doStatement(tokens)
    | "return" -> returnStatment(tokens)

and returnStatment(tokens:tokenRecord[]) = 
    let curr = tokens.[index]
    index <- index + 1
    let next = tokens.[index]
    let mutable r = []
  
    //now check if the code give back an expression
    if next.value = ";" then 
        r <- [tokenToParserRecord(curr)] @ [tokenToParserRecord(next)]
        index <- index + 1
    else 
        r <- [tokenToParserRecord(curr)] @ expression(tokens)
        let fianl = tokens.[index]
        index <- index + 1
        r <- r @ [tokenToParserRecord(fianl)]
    [{pType = ReturnStatement; inner = r; value = "returnStatement"}]

 and doStatement(tokens:tokenRecord[]) =
    let curr = tokens.[index] //'do' token
    index <- index + 1
    let subCall = subroutineCall(tokens)
    let fianl = tokens.[index] // ';' token
    index <- index + 1

    [{pType = DoStatement; inner = [tokenToParserRecord(curr)] @ subCall @ [tokenToParserRecord(fianl)]; value = ""}]

and whileStatement(tokens:tokenRecord[]) =
    //the while with (expression)
    let curr = tokens.[index]
    index <- index + 1
    let openB = tokens.[index]
    index <- index + 1
    let expre = expression(tokens)
    let closeB = tokens.[index]
    index <- index + 1

    //the statements (with {})
    let openS = tokens.[index]
    index <- index + 1
    let Statements = statements(tokens)
    let closeS = tokens.[index]
    index <- index + 1

    let r = [tokenToParserRecord(curr)] @ [tokenToParserRecord(openB)] @ expre @ [tokenToParserRecord(closeB)] @ [tokenToParserRecord(openS)] @ Statements @ [tokenToParserRecord(closeS)]
    [{pType = WhileStatement; inner = r; value = ""}]

and ifStatement(tokens:tokenRecord[]) =
   //the while with (expression)
    let curr = tokens.[index]
    index <- index + 1
    let openB = tokens.[index]
    index <- index + 1
    let expre = expression(tokens)
    let closeB = tokens.[index]
    index <- index + 1

    //the statements (with {})
    let openS = tokens.[index]
    index <- index + 1
    let Statements = statements(tokens)
    let closeS = tokens.[index]
    index <- index + 1

    //the statement so far
    let mutable r = [tokenToParserRecord(curr)] @ [tokenToParserRecord(openB)] @ expre @ [tokenToParserRecord(closeB)] @ [tokenToParserRecord(openS)] @ Statements @ [tokenToParserRecord(closeS)]
    
    //check if there is else 
    let elseS = tokens.[index]
    if(elseS.value = "else") then
        index <- index + 1
        let elseOpenS = tokens.[index]
        index <- index + 1
        let elseStatements = statements(tokens)
        let elseCloseS = tokens.[index]
        index <- index + 1

        r <- r @ [tokenToParserRecord(elseS)] @ [tokenToParserRecord(elseOpenS)] @ elseStatements @ [tokenToParserRecord(elseCloseS)]
     
    [{pType = IfStatement; inner = r; value = ""}]

and letStatement(tokens:tokenRecord[]) =
    let letS = tokens.[index]
    index <- index + 1
    let varName = tokens.[index]
    index <- index + 1
    
    let next = tokens.[index]
    index <- index + 1

    let mutable r = [tokenToParserRecord(letS)] @ [tokenToParserRecord(varName)] @ [tokenToParserRecord(next)]

    //check if there is also ('['expression']') part
    if(next.value = "[") then
        let expre = expression(tokens)
        let closeB = tokens.[index]
        index <- index + 1
        let equal = tokens.[index]
        index <- index + 1

        r <- r @ expre @ [tokenToParserRecord(closeB)] @ [tokenToParserRecord(equal)]

    //the part after the '='
    let expreS = expression(tokens)
    let fianl = tokens.[index]
    index <- index + 1
    
    [{pType = LetStatement; inner = r @ expreS @ [tokenToParserRecord(fianl)]; value = ""}]
    



let varName(tokens:tokenRecord[]) =
    let curr = tokens.[index]
    index <- index + 1

    [tokenToParserRecord(curr)]

let className(tokens:tokenRecord[]) =
    let curr = tokens.[index]
    index <- index + 1

    [tokenToParserRecord(curr)]

let typeStructure(tokens:tokenRecord[]) =
    let curr = tokens.[index]
    index <- index + 1
    [tokenToParserRecord(curr)]

let varDec(tokens:tokenRecord[]) =
    let var = tokens.[index] //'var' token
    index <- index + 1
    
    let mutable r = []
    r <- [tokenToParserRecord(var)] @ typeStructure(tokens) @ varName(tokens)

    let mutable curr = tokens.[index]
    //going over all the vars declrations
    while curr.value = "," do
        index <- index + 1
        r <- r @ [tokenToParserRecord(curr)] @ varName(tokens)
        curr <- tokens.[index]
     
    r <- r @ [tokenToParserRecord(tokens.[index])] //';' token
    index <- index + 1

    [{pType = VarDec; inner = r; value = ""}]   

let subroutineBody(tokens:tokenRecord[]) =
    let openB = tokens.[index]
    index <- index + 1

    let mutable r = []

    while tokens.[index].value = "var" do
        r <- r @ varDec(tokens)

    let state = statements(tokens)
    let closeB = tokens.[index]
    index <- index + 1

    [{pType = SubroutineBody; inner = [tokenToParserRecord(openB)] @ r @ state @ [tokenToParserRecord(closeB)]; value = ""}]
    
let paramaterList(tokens:tokenRecord[]) =
    let mutable curr = tokens.[index]
    let mutable r = []

    //check if there are any parameters
    if isType(curr) then
        r <- typeStructure(tokens) @ varName(tokens)
        curr <- tokens.[index]
        while curr.value = "," do
            index <- index + 1
            r <- r @ [tokenToParserRecord(curr)] @ typeStructure(tokens) @ varName(tokens)
            curr <- tokens.[index]

    [{pType = ParamaterList; inner = r; value = ""}]
   
let subroutineDec(tokens:tokenRecord[]) =
    let declartion = tokens.[index]
    index <- index + 1
    let mutable r = []

    match declartion.value with
    | "constructor" -> r <- [tokenToParserRecord(declartion)] 
    | "function" -> r <- [tokenToParserRecord(declartion)]
    | "method" -> r <- [tokenToParserRecord(declartion)]

    let kind = tokens.[index]

    if kind.value = "void" then
        r <- r @ [tokenToParserRecord(kind)]
        index <- index + 1
    else if isType(kind) then
        r <- r @ typeStructure(tokens)
     
    r <- r @ subroutineName(tokens)

    let openS = tokens.[index]
    index <- index + 1
    let PL = paramaterList(tokens)
    let closeS = tokens.[index]
    index <- index + 1
    
    [{pType = SubroutineDec; inner = r @ [tokenToParserRecord(openS)] @ PL @ [tokenToParserRecord(closeS)] @ subroutineBody(tokens); value = ""}]
    
let classVarDec(tokens:tokenRecord[]) =
    let mutable r = []
    let kind = tokens.[index] // static|field
    index <- index + 1

    r <- [tokenToParserRecord(kind)] @ typeStructure(tokens) @ varName(tokens)

    let mutable curr = tokens.[index] 
    while curr.value = "," do
        index <- index + 1
        r <- r @ [tokenToParserRecord(curr)] @ varName(tokens)
        curr <- tokens.[index]

    let final = tokens.[index] //the ';'
    index <- index + 1

    [{pType = ClassVarDec; inner = r @ [tokenToParserRecord(final)]; value = ""}]

let classStructure(tokens:tokenRecord[]) = 
    index <- 0
    let mutable r = []
    let s = tokens.[index] //the 'class' token
    index <- index + 1

    r <- [tokenToParserRecord(s)] @ className(tokens) @ [tokenToParserRecord(tokens.[index])] //including the '{'
    index <- index + 1

    while isClassVarDec(tokens.[index]) do
        r <- r @ classVarDec(tokens)

    while isSubroutineDec(tokens.[index]) do
        r <- r @ subroutineDec(tokens)

    let fianl = tokens.[index]
    index <- index + 1

    [{pType = Class; inner = r @ [tokenToParserRecord(fianl)]; value = ""}]
