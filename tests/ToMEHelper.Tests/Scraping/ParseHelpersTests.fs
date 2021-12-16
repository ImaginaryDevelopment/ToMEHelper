module ToMEHelper.Scraping.ParseHelpersTests

open System
open Expecto
open TestHelpers

// open Expecto.ExpectoFsCheck
// open ToMEHelper
// open FSharp.Reflection
open ToMEHelper.Schema
open ToMEHelper.Scraping.ParseHelpers
open ToMEHelper.BHelpers

[<Tests>]
let uncategorizedTests =
    let divText = "hi"
    let divId = "mydiv"
    let div = sprintf "<div id=\"%s\">%s</div>" divId divText
    let emptyNode = "<div id=\"\"></div>"
    let htmlWithBody = sprintf "<html><body id=\"hello\">%s%s</body></html>" div emptyNode
    testList "ParseHelpers" [
        testCase "parseHtml can be happy"
        <| fun _ ->
            let htmldoc = lazy(parseHtml htmlWithBody)
            htmldoc.Force() |> ignore
        testCase "getElementById can be happy"
        <| fun _ ->
            let htmldoc = lazy(parseHtml htmlWithBody)
            htmldoc.Value
            |> getElementById "hello"
            |> flip Expect.isNotNull null
        testCase "getOuterHtml can be happy"
        <| fun _ ->
            let htmldoc = lazy(parseHtml htmlWithBody)
            let expected = div
            let actual = htmldoc.Value |> getElementById divId |> getOuterHtml
            Expect.equal actual expected null
        testCase "getText can be happy"
        <| fun _ ->
            let htmldoc = lazy(parseHtml htmlWithBody)
            let expected = divText
            let actual = htmldoc.Value |> getElementById divId |> getText
            Expect.equal actual expected null

        // this test seems to never terminate
        testCase "getChildren can be happy"
        <| fun _ ->
            let htmldoc = lazy(parseHtml htmlWithBody)
            let expected = div
            let actual =
                async {
                 return htmldoc.Value.DocumentNode
                |> getElement "html"
                |> Option.bind(getElement "body")
                |> Option.bind(getChildren >> Seq.tryHead >> Option.map getOuterHtml)
                }
                |> Async.withTimeout 1500
                |> Async.RunSynchronously
            match actual with
            | Ok(Some actual) -> Expect.equal actual expected null
            | Error e -> failwith "Timeout exceeded"
            | Ok None -> failwith "no children found"

        // testCase "getAttrValue can be happy"
        // <| fun _ ->
        //     let expected = divId
        //     let actual = htmldoc.Value |> getElementById divId |> getAttrValue "id"
        //     match actual with
        //     | Some actual -> Expect.equal actual expected null
        //     | None -> failwith "no matching attr value found"
    ]




// let getElement (key:string) (x:HtmlNode) =
//     x.ChildNodes
//     |> Seq.tryFind (fun n -> n.Name = key)
// let getElements (key:string) (x:HtmlNode) =
//     x.ChildNodes
//     |> Seq.filter(fun n -> n.Name = key)
//     |> List.ofSeq

// let getHrefWithInner (x:HtmlNode):string option * string = x |> getAttrValue "href", x.InnerText

// let (| GetElement |_|) (name:string) x =
//     getElement name x

// let (|NodeName|_|) name (x:HtmlAgilityPack.HtmlNode) =
//     if x.Name = name then Some x else None
