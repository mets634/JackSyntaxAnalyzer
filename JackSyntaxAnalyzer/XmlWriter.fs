module XmlWriter

open System
open System.IO
open System.Xml.Linq
open System.Xml
open TokenType

(*A module to write things to
XML files.*)

let createTokensXml(tokens:tokenRecord[]) =
        // build xml tree
        let xmlTokens = tokens |> 
                        Array.filter (fun t -> t.eType < IgnoreMe) |> // not an ignore me
                        Array.map (fun t -> new XElement(XName.Get(elementTypeToString(t.eType)), t.value)) 
        let root = new XElement(XName.Get("tokens"), xmlTokens)

        root // return the xml structure