module XmlWriter


open System.Xml.Linq
open TokenType

(*A module to write things to
XML files.*)

let writeTokens(tokens:tokenRecord[]) =
        let xmlTokens = tokens |> Array.map (fun t -> new XElement(XName.Get(t.eType.ToString()), t.value))
        let root = new XElement(XName.Get("tokens"), xmlTokens)

        root.Save(@"tokens.xml") // write to file