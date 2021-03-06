﻿module ParserType

open TokenType

type parserType = KeyWord | Symbol | IntegerConstant | StringConstant | Identifier // lexical elements
                    | Expression | Term | SubroutineCall | ExpressionList // expressions
                    | ReturnStatement | DoStatement | WhileStatement | IfStatement | LetStatement | Statements //statements
                    | VarDec | SubroutineBody | ParamaterList | SubroutineDec | ClassVarDec | Class //structure
                    | IgnoreMe

let tokenToParser (t:elementType) =
    match t with
    | elementType.KeyWord -> KeyWord
    | elementType.Symbol -> Symbol
    | elementType.IntegerConstant -> IntegerConstant
    | elementType.StringConstant -> StringConstant
    | elementType.Identifier -> Identifier
    | _ -> IgnoreMe

let parserTypeToString (e:parserType) =
    match e with
    // lexical elements
    | KeyWord -> "keyword"
    | Symbol -> "symbol"
    | IntegerConstant -> "integerConstant"
    | StringConstant -> "stringConstant"
    | Identifier -> "identifier"

    // expressions
    | Expression -> "expression"
    | Term -> "term"
    | SubroutineCall -> "subroutineCall"
    | ExpressionList -> "expressionList"

    //statements
    | ReturnStatement -> "returnStatement"
    | DoStatement -> "doStatement"
    | WhileStatement -> "whileStatement"
    | IfStatement -> "ifStatement"
    | LetStatement -> "letStatement"
    | Statements -> "statements"

    //structure
    | VarDec ->"varDec"
    | SubroutineBody -> "subroutineBody"
    | ParamaterList -> "paramaterList"
    | SubroutineDec -> "subroutineDec"
    | ClassVarDec -> "classVarDec"
    | Class -> "class"

    | _ -> ""

type parserRecord = { pType:parserType; inner:parserRecord list; value:string } 

let tokenToParserRecord (t:tokenRecord) =
    { pType=tokenToParser(t.eType); inner=List.empty; value=t.value }

let mutable varList = List<string>.Empty
let mutable subroutineList = List<string>.Empty
let mutable classList = List<string>.Empty

let isIntegerConstant (tkn:tokenRecord) =
    match tkn.eType with
    | elementType.IntegerConstant -> true
    | _ -> false

let isStringConstant (tkn:tokenRecord) =
    match tkn.eType with
    | elementType.StringConstant -> true
    | _ -> false

let isKeywordConstant (tkn:tokenRecord) =
    match tkn.value with
    | "true" | "false" | "null" | "this" -> true
    | _ -> false

let isVarName (tkn:tokenRecord) =
    match tkn with
    | t when t.eType = elementType.Identifier -> true
    | _ -> false

let isClassName (tkn:tokenRecord) =
    match tkn with
    | t when t.eType = elementType.Identifier -> true
    | _ -> false

let isSubroutineName (tkn:tokenRecord) =
    match tkn with
    | t when t.eType = elementType.Identifier -> true
    | _ -> false

let isUnaryOp (tkn:tokenRecord) =
    match tkn.value with
    | "-" | "~" -> true
    | _ -> false

let isOp (tkn:tokenRecord) =
    match tkn.value with
    | "+" | "-" | "*" | @"/" | "&" | "|" | "<" | ">" | "=" -> true
    | _ -> false

let isType(tkn:tokenRecord) = 
    match tkn.value with
    | "int" -> true
    | "char" -> true
    | "boolean" -> true
    | _ -> if isClassName(tkn) then true
           else false

let isClassVarDec(tkn:tokenRecord) = 
    match tkn.value with
    | "static" -> true
    | "field" -> true
    | _ -> false

let isSubroutineDec(tkn:tokenRecord) = 
    match tkn.value with
    | "constructor" -> true
    | "function" -> true
    | "method" -> true
    | _ -> false

exception ParserException of string

