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

    // tests for a simple int -> Result<'t,int> value
    let makeMapOkTests f okValues =
        okValues
        |> List.map(fun i ->
            testCase (string i)
            <| fun _ ->
                match f i with
                | Ok _ -> ()
                | Error e -> failwith (string e)
        )
    let makeMapErrorTest f =
        fun _ ->
            let expected = Error -1
            let actual = f -1
            Expect.equal actual expected null

    testList "getClassFromId" [
        let classes =
            FSharp.Reflection.FSharpType.GetUnionCases(typeof<ToMEClass>)
            |> Array.map(fun uc -> uc.Name, FSharp.Reflection.FSharpValue.MakeUnion(uc,Array.empty) :?> ToMEClass)
        yield! classes |>
            Array.map(fun (n,v) ->
                testCase n
                <| fun _ ->

                    let cid = getClassId v
                    match getClassFromId cid with
                    | Ok actual ->
                        Expect.equal actual v null
                    | Error e -> failwithf "getClassFromId expected %A but was %A" v e
            )
        testCase "can be unhappy" <| makeMapErrorTest getClassFromId
    ]
    testList "getCampaignFromId" [
        yield! makeMapOkTests getCampaignFromId [2;46;24;67402]
        testCase "can be unhappy" <| makeMapErrorTest getCampaignFromId
    ]
    testList "getDifficultyFromId" [
        yield! makeMapOkTests getDifficultyFromId [33;6;26;227;36]
        testCase "can be unhappy" <| makeMapErrorTest getDifficultyFromId
    ]
    testList "getPermadeathFromId" [
        yield! makeMapOkTests getPermadeathFromId [72;65;66]
        testCase "can be unhappy" <| makeMapErrorTest getPermadeathFromId
    ]
    testList "getRaceFromId" [
        yield! makeMapOkTests getRaceFromId [114;133993;25;42;104070;74;21;47;67497;18;3;9;8;23296;37515;13]
        testCase "can be unhappy" <| makeMapErrorTest getRaceFromId
    ]
]

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