module ToMEHelper.Scraping.SiteSchemaTests

open System
open Expecto

open ToMEHelper.Schema

open ToMEHelper.Scraping.ApiHelpers
open ToMEHelper.Scraping.SiteSchema

open TestHelpers

let races = lazy(ToMEHelper.SchemaTests.makeDUAll<ToMERace>())
let classes = lazy(ToMEHelper.SchemaTests.makeDUAll<ToMEClass>())

[<Tests>]
let uncategorizedTests = testList "ApiHelpers" [

]

[<Tests>]
let getRaceId =
    testList "getRaceId" (
        races.Value
        |> Seq.map(fun x -> testCase (string x) (fun _ -> getRaceId x |> ignore))
        |> List.ofSeq
    )
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