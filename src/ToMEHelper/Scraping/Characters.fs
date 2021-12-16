/// Functionality for scraping character sheets
namespace ToMEHelper.Scraping.Characters

open ToMEHelper.BHelpers

module Logging =
    let mutable logger : ToMELogger option = None
    let inline dump (x:'t) =
        logger
        |> Option.iter(fun logger ->
            logger.Dump(x)
        )
    let inline dumpt title x =
        logger
        |> Option.iter(fun logger ->
            logger.Dump(x,description=title)
        )

open ToMEHelper.Scraping.ParseHelpers

module Parsing =
    open ToMEHelper.Schema
    let parseTalent data spentTd =
        match data with
        | NodeName "td" (GetElement "ul" ul) ->
            let talentName =
                let last = getElements "li" ul |> Seq.last
                last |> getOuterHtml |> after "</div>" |> before "<" |> trim
            // what about talents that go higher or lower than 5?
            let value:Talent = {Name=talentName;Points = spentTd |> getText |> before "/5" |> int}
            Ok value
        | _ -> Error("nonmatch",data |> getOuterHtml)

    let mapTalents el: (TalentCategory * _) seq=
        match el |> getElement "table" with
        | None -> failwith "unable to find talent table"
        | Some x -> x
        |> getElements "tr"
        |> List.ofSeq
        |> chunkBy (function | GetElement "td" (GetElement "strong" _) -> true | _ -> false)
        |> Seq.choose (List.map getChildren >> (fun x ->
            let inline dumpNodes title =
                    x |> List.map (List.map getOuterHtml) |> Logging.dumpt title
            let (|StrongText|_|) =
                function
                |  GetElement "strong" x ->
                x |> getText |> Some
                | x ->  printfn "StrongText didn't match %A" (getOuterHtml x);None
            let getTitle (nodes: _ list) =
                match nodes with
                | NodeName "td" (StrongText n)::NodeName "td" el ::[] -> Ok(n, getText el)
                | _ ->
                    nodes |> List.map getOuterHtml |> Logging.dumpt "title did not match"
                    Error "title not found"
            let (|Title|_|) (nodes: _ list) = match getTitle nodes with | Ok x -> Some x | _ -> None
            try
                match x with
                | Title (cat,pwr) :: rem ->

                    let paired = rem |> List.map(function | a::b::[] -> a,b | _ -> failwith "pairing failed" )
                    let talents = paired |> List.map (fun (a,b) -> parseTalent a b)
                    let good = talents |> List.choose (function | Ok x -> Some x | _ -> None)
                    let bad = talents |> List.choose (function | Error x -> Some x | _ -> None) |> List.map(fun (e1,e2) -> Error [e1;e2])
                    Some ({Name=cat;Power=pwr;Talents= good},bad)
                | _ ->
                    dumpNodes "failing uhoh"
                    None

            with ex ->
                dumpNodes "failing "
                ex |> Logging.dump
                failwithf "Failed to map talents: %s" ex.Message
        ))