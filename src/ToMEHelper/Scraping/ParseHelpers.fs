module ToMEHelper.Scraping.ParseHelpers

open ToMEHelper.BHelpers

// open ToMEHelper.Schema
open HtmlAgilityPack

let parseHtml (x:string) =
    let d = HtmlAgilityPack.HtmlDocument()
    d.LoadHtml x
    d

let getElementById x (d:HtmlDocument) = d.GetElementbyId x
let getOuterHtml (x:HtmlNode) = x.OuterHtml
let getText (x:HtmlNode) = x.InnerText
let getChildren (x:HtmlNode) = x.ChildNodes |> List.ofSeq
let getAttrValue key (x:HtmlNode): string option=
    match x.GetAttributeValue(key,null) with
    | ValueString x -> Some x
    | _ -> None

let getElement (key:string) (x:HtmlNode) =
    x.ChildNodes
    |> Seq.tryFind (fun n -> n.Name = key)
let getElements (key:string) (x:HtmlNode) =
    x.ChildNodes
    |> Seq.filter(fun n -> n.Name = key)
    |> List.ofSeq

let getHrefWithInner (x:HtmlNode):string option * string = x |> getAttrValue "href", x.InnerText

let (| GetElement |_|) (name:string) x =
    getElement name x

let (|NodeName|_|) name (x:HtmlAgilityPack.HtmlNode) =
    if x.Name = name then Some x else None
