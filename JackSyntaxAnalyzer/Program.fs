﻿module main

open System
open System.IO
open System.Text.RegularExpressions

open XmlWriter
open Tokenizer
open expression
open TokenType
open expression
open TokenIndex
open CodeGeneration


let tempFile = @"temp.jack"


let stripComments(file:FileInfo) = 
    let singleLineCommentRegex = @"//.*"
    let multiLineCommentRegex = @"/\*(.|[\r\n])*?\*/"

    // remove comments
    let noMultiComments = Regex.Replace ((File.ReadAllText file.FullName), multiLineCommentRegex, "")
    let noComments = Regex.Replace (noMultiComments, singleLineCommentRegex, "", RegexOptions.Multiline)

    File.WriteAllText(tempFile, noComments) 


let getArg (argv:string[]) =
        match argv.Length with
        // no input, prompt user for path
        | 0 -> printfn "ENTER DIRECTORY PATH:\n\n"
               Console.ReadLine()
        // got input
        | _ -> argv.[0]


let compileFile(file:FileInfo) = 
    stripComments(file) // writes the temp file

    // tokenize file
    let sr = new StreamReader(tempFile)
    let cmds = tokenize(sr) |> 
               List.toArray |> 
               Array.filter(fun t -> t.eType < elementType.IgnoreMe) |>
               classStructure |>
               List.item(0) |>
               classGenerate |>
               List.map (fun str -> str + Environment.NewLine) |> // add endl to each command
               String.Concat

    // write to token file
    File.WriteAllText(file.DirectoryName + @"\" + Path.GetFileNameWithoutExtension(file.Name) + @".vm", cmds)

    sr.Close()

[<EntryPoint>]
let main argv = 
    let dirPath = getArg argv

    let dir = new DirectoryInfo(dirPath)
    dir.GetFiles() // get directory files
    |> Array.filter (fun f -> f.Extension = ".jack") // is a jack file
    |> Array.iter (fun f -> compileFile f) // tokenize jack file

    try
        File.Delete(tempFile)
    with _ -> Console.WriteLine("CANNOT DELETE TEMP FILE")

    Console.WriteLine("DONE...")
    Console.ReadKey() |> ignore

    0 // return an integer exit code
