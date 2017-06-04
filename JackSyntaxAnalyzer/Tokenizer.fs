module Tokenizer

open System
open System.IO
open LexicalElements

(*This module represents Jack's 
tokenizer. The tokenizer is a FSM 
who is described in the word 
document of this project.*)

type elementType = KeyWord | Symbol | IntegerConstant | StringConstant | Identifier
type tokenRecord = { eType:elementType; value:string }


let Q6 (str:string) = { eType=elementType.StringConstant; value=str }

let Q5 (character:Char) = 

let Q4 (sym:Char) = { eType=elementType.Symbol; value=sym }

//let Q3 (character:Char) = 
//
//let Q2 (character:Char) = 
//
//let Q1 (word:string) = 

let Q0 (f:StreamReader) = 
    let curr = ""
    match f.Read() with
    | 