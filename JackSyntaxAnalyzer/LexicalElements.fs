module LexicalElements

open System


(*In this module, each type of element
is given a predicate to determine if a 
given string a of that element. The predicate
is in the following format: isPredicateName*)


/// KEYWORD ///

type keyWordType =
                | CLASS
                | CONSTRUCTOR
                | FUNCTION
                | METHOD
                | FIELD
                | STATIC
                | VAR
                | INT
                | CHAR
                | BOOLEAN
                | VOID
                | TRUE
                | FALSE
                | NULL
                | THIS
                | LET
                | DO
                | IF
                | ELSE
                | WHILE
                | RETURN


let keyWordList = ["class"; "constructor"; "function";
                    "method"; "field"; "static"; "var";
                    "int"; "char"; "boolean"; "void"; "true";
                    "false"; "null"; "this"; "let";
                    "do"; "if"; "else"; "while"; "return"]

let isKeyWord (str:string) = 
    List.contains str keyWordList


/// SYMBOL ///

let symbolList = ["{"; "}"; "("; ")"; "["; "]";
                   ","; "."; ";"; "+"; "-"; "*"; @"/";
                   "&"; "|"; "<"; ">"; "="; "~"]

let isSymbol (str:string) = 
    List.contains str symbolList


/// INTEGER CONSTANT ///

let isIntegerConstant (str:string) =
    try
        let number = Int32.Parse str
        number >= 0 && number < 32767

    with _ -> false  // not a valid integer


/// STRING CONSTANT ///

let 