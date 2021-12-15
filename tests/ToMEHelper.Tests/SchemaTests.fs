module ToMEHelper.SchemaTests

open System
open Expecto
open Expecto.ExpectoFsCheck
// open ToMEHelper
open FSharp.Reflection

open Schema
let makeDUAll<'t>() =
    FSharp.Reflection.FSharpType.GetUnionCases(typeof<'t>)
    |> Array.map(fun x -> FSharpValue.MakeUnion(x, Array.empty) :?> 't)
let makeDUAll1<'t,'targ> value =
    FSharp.Reflection.FSharpType.GetUnionCases(typeof<'t>)
    |> Array.map(fun x -> FSharpValue.MakeUnion(x, Array.singleton value) :?> 't)

let makeAlwaysTest<'t> (all:'t[] option) name f =
    let all =
        match all with
        | None -> makeDUAll<'t>()
        | Some x -> x
    testCase name
    <| fun _ ->
        all
        |> Seq.iter(f)




[<Tests>]
let tomeRaceTests =
    let all = makeDUAll<ToMERace>()
    ()
    testList "ToMERace" [
        testCase "getDisplay is always happy"
        <| fun _ ->
            all
            |> Array.iter(fun x -> x.GetDisplay() |> ignore<string>)
            ()
    ]

[<Tests>]
let tomeClassTests =
    let all = makeDUAll<ToMEClass>()
    testList "ToMEClass" [
        testCase "getDisplay is always happy"
        <| fun _ ->
            all
            |> Array.iter(fun x -> x.GetDisplay() |> ignore<string>)
            ()
    ]


[<Tests>]
let talentTests =
    testList "talent" [
        testProperty "toDump doesn't throw"
        <| fun (name,points) ->
            let x : Talent = {Name=name;Points=points}
            x.ToDump()
            |> ignore<string>
    ]

[<Tests>]
let talentCategoryTypeTests =
    makeAlwaysTest<TalentCategoryType> None "talentCategory" (fun x -> x.ToDump() |> ignore<string>)

[<Tests>]
let talentPowerTypeTests =
    let all = makeDUAll1<TalentPower,_> ""
    makeAlwaysTest<TalentPower> (Some all) "talentPower" (fun x -> x.ToDump() |> ignore<string>)

// module AggregationTests =
//     open Aggregation

//     [<Tests>]
//     let tests =
//         testList "Aggregation" [
//             testList "CategoryInvestment" [
//                 testProperty "toDump"
//                 <| fun (x:CategoryInvestment) ->
//                     x.ToDump() |> ignore<int>
//             ]
//         ]


