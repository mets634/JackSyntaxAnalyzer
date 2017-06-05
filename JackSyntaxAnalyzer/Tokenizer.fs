module Tokenizer

open System
open System.IO
open LexicalElements
open TokenType

(*This module represents Jack's 
tokenizer. The tokenizer is a FSM 
who is described in the word 
document of this project. Notice that
we merged part of Q1 into Q2.*)

let Q6 (prev:string) = { eType=elementType.StringConstant; value=prev }


let rec Q5 (sr:StreamReader, prev:string) = 
        try
            let next = sr.Read() |> Convert.ToChar // read character
            let currToken = (prev, next.ToString()) |> String.Concat // add 'next' to the current token

            match next with
            | '\n' -> failwith "EXPECTED CLOSING QUOTE, GOT NEW-LINE INSTEAD -> " + string // new-line not legal
            | '\"' -> Q6 currToken // got closing quote
            | _ -> Q5 (sr, currToken) // next step

        with :? System.OverflowException -> failwith "EXPECTED CHARACTER GOT EOF -> " + string // got -1, EOF


let Q4 (prev:string) = { eType=elementType.Symbol; value=prev }


let rec Q3 (sr:StreamReader, prev:string) = 
        try
            let next = sr.Peek() |> Convert.ToChar // peak at next character
            let currToken = (prev, (next.ToString())) |> String.Concat // append character to current token
            match next with
            | c when Char.IsNumber(c) -> sr.Read() |> ignore // get rid of peeked character
                                         Q3(sr, currToken)  // move to next character
            | _ -> { eType=elementType.IntegerConstant; value=prev } // got something else, return token

        with :? System.OverflowException -> { eType=elementType.IntegerConstant; value=prev } // got -1, EOF


let Q1 (prev:string) = { eType=elementType.KeyWord; value=prev }


let rec Q2 (sr:StreamReader, prev:string) = 
        try
            let next = sr.Peek() |> Convert.ToChar // peak at next character
            let currToken = (prev, (next.ToString())) |> String.Concat // append character to current token
            match next with
            | c when (Char.IsControl next) || // control char
                      (String.Equals ((next.ToString()), " ")) && // or a space
                     isKeyWord(prev) -> Q1(prev)  // is a keyword, get keyword token

            | c when (Char.IsControl next) || // control char
                      (String.Equals ((next.ToString()), " ")) -> { eType=elementType.Identifier; value=prev } // an identifier

            | c when (Char.IsLetterOrDigit next) || (String.Equals ((next.ToString()), "_")) -> sr.Read() |> ignore // use next character
                                                                                                Q2 (sr, currToken)

            | _ -> failwith "EXPECTED LETTER, UNDERSCORE, or DIGIT. GOT " + next + " -> " + prev

        with :? System.OverflowException -> failwith "EXPECTED CHARACTER GOT EOF -> " + prev // got -1, EOF


let rec Q0 (sr:StreamReader) = 
    let next = sr.Read() |> Convert.ToChar // read next character
    match next with
    | '\"' -> Q5(sr, next.ToString()) // start of string
    | _ when isSymbol(next) -> Q4(next.ToString()) // symbol
    | _ when Char.IsDigit(next) -> Q3(sr, next.ToString()) // digit
    | _ when Char.IsLetter(next) || (String.Equals (next.ToString(), "_")) -> Q2(sr, next.ToString())
    | _ when Char.IsControl(next) || (String.Equals (next.ToString(), " ")) -> Q0(sr) // control character (or space). ignore
    | _ -> failwith "UNIDENTIFIED CHARACTER " + next.ToString()