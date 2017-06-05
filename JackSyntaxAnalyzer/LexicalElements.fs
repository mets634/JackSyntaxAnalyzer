module LexicalElements

open System


(*In this module, some types of elements
are given a predicate to determine if a 
given string a of that element. The predicate
is in the following format: isPredicateName*)


/// KEYWORD ///

let keyWordList = ["class"; "constructor"; "function";
                    "method"; "field"; "static"; "var";
                    "int"; "char"; "boolean"; "void"; "true";
                    "false"; "null"; "this"; "let";
                    "do"; "if"; "else"; "while"; "return"]

let isKeyWord (str:string) = 
    List.contains str keyWordList


/// SYMBOL ///

let symbolList = ['{'; '}'; '('; ')'; '['; ']';
                   ','; '.'; ';'; '+'; '-'; '*'; '/';
                   '&'; '|'; '<'; '>'; '='; '~']

let isSymbol (c:Char) = 
    List.contains c symbolList