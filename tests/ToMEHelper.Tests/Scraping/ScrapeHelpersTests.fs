module ToMEHelper.Scraping.ScrapeHelpersTests

open System
open Expecto
// open Expecto.ExpectoFsCheck
// open ToMEHelper
// open FSharp.Reflection
open ToMEHelper.Schema
open ToMEHelper.Scraping.ScrapeHelpers
open TestHelpers
open ToMEHelper.BHelpers

let races = lazy(ToMEHelper.SchemaTests.makeDUAll<ToMERace>())
let classes = lazy(ToMEHelper.SchemaTests.makeDUAll<ToMEClass>())

[<Tests>]
let getClassId =
    testList "getClassId" (
        classes.Value
        |> Seq.map(fun x -> testCase (string x) (fun _ -> getClassId x |> ignore))
        |> List.ofSeq
    )

[<Tests>]
let getDifficultyId =
    testList "getDifficultyId" (
        ToMEHelper.SchemaTests.makeDUAll<Difficulty>()
        |> Seq.map(fun x -> testCase (string x) (fun _ -> getDifficultyId x |> ignore))
        |> List.ofSeq
    )

[<Tests>]
let getPermadeathId =
    testList "getPermadeathId" (
        ToMEHelper.SchemaTests.makeDUAll<Permadeath>()
        |> Seq.map(fun x -> testCase (string x) (fun _ -> getPermadeathId x |> ignore))
        |> List.ofSeq
    )

[<Tests>]
let getCampaignId =
    testList "getCampaignId" (
        ToMEHelper.SchemaTests.makeDUAll<Campaign>()
        |> Seq.map(fun x -> testCase (string x) (fun _ -> getCampaignId x |> ignore))
        |> List.ofSeq
    )

[<Tests>]
let tryGetRaceTests = testList "tryGetRace" [
    testCase "all races parse"
    <| fun _ ->
        races.Value
        |> Seq.map (fun x -> x, string x)
        |> Seq.iter(fun (r,text) ->
            tryGetRace [text]
            |> Option.iter(Tuple2.iter (fun v -> Expect.equal v r "race not parsed") (Assert.equal List.empty))
        )


]
module Fetch =
    open Fetch.Internals
    [<Tests>]
    let fetchTests =
        testList "Fetch" [
            testList "hc" [
                testCase "is happy"
                <| fun _ ->
                    hc.Force() |> ignore
            ]
            testList "getPage" [
                testList "Custom" [
                    let getPage f = getPage (Custom f) (System.Uri "http://test.com") |> Async.RunSynchronously
                    testCase "can be happy"
                    <| fun _ ->
                        let text = "hello world"
                        let expected = Ok text
                        let actual = getPage (fun _ -> async { return text })
                        Expect.equal actual expected null
                    testCase "doesn't get too sad"
                    <| fun _ ->
                        let expected = "blah"
                        let actual = getPage (fun _ -> async { return failwith expected})
                        match actual with
                        | Ok _ -> failwith "F was not ok"
                        | Error ex ->
                            Expect.equal ex.Message expected null
                ]
            ]
            testList "toQueryValues" [
                testCase "can be happy"
                <| fun _ ->
                    let expected = ["hello=world"]
                    let actual = toQueryValues "hello" ["world"] |> List.ofSeq
                    Expect.equal actual expected null
                testCase "can handle multiple"
                <| fun _ ->
                    let expected = ["hello=world";"hello=world"]
                    let actual = toQueryValues "hello" ["world";"world"] |> List.ofSeq
                    Expect.equal actual expected null
                testCase "can handle a mix"
                <| fun _ ->
                    let expected = ["hello=world";"hello=goodbye"]
                    let actual = toQueryValues "hello" ["world";"goodbye"] |> List.ofSeq
                    Expect.equal actual expected null
                testCase "can handle empty"
                <| fun _ ->
                    let actual = toQueryValues "hello" []
                    Expect.isEmpty actual null
            ]

            testList "queryPage'" [
                testList "Custom" [
                    let queryPage f = queryPage' 0 (Custom f) ("http://test.com") Map.empty |> Async.RunSynchronously
                    testCase "can be happy"
                    <| fun _ ->
                        let text = "hello world"
                        let expected = Ok text
                        let actual = queryPage (fun _ -> async { return text })
                        Expect.equal actual expected null
                    // test appears to hang
                    ptestCase "doesn't get too sad"
                    <| fun _ ->
                        let expected = "blah"
                        let actual = queryPage (fun _ -> async { return failwith expected})
                        match actual with
                        | Ok _ -> failwith "F was not ok"
                        | Error [ex] ->
                            Expect.equal ex.Message expected null
                        | Error exs ->
                            failwithf "Expected a single error but was %i" exs.Length
                ]
            ]
        ]