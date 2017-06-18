module XmlWriter

open System
open System.IO
open System.Xml.Linq
open System.Xml

open TokenType
open ParserType

(*A module to write things to
XML files.*)

let createTokensXml(tokens:tokenRecord[]) =
        // build xml tree
        let xmlTokens = tokens |> 
                        Array.filter (fun t -> t.eType < elementType.IgnoreMe) |> // not an ignore me
                        Array.map (fun t -> new XElement(XName.Get(elementTypeToString(t.eType)), t.value)) 
        let root = new XElement(XName.Get("tokens"), xmlTokens)

        root // return the xml structure

let rec createParserXml(tree:parserRecord list) =
    let xml = tree |>
              List.toArray |>
              Array.filter (fun p -> p.pType < IgnoreMe) |> // not an ignore me
              Array.map (fun p -> createSingleParserTree(p))// parse each item

    xml

and createSingleParserTree(parse:parserRecord) =
    match parse.inner.Length with
    | 0 -> new XElement(XName.Get(parserTypeToString(parse.pType)), parse.value)
    | _ -> new XElement(XName.Get(parserTypeToString(parse.pType)), createParserXml(parse.inner))