module ToMEHelper.BHelpersTests

open System
open Expecto
open ToMEHelper
open ToMEHelper.BHelpers

[<Tests>]
let uncategorizedTests =
    testList "uncategorized" [
        testCase "flip does flip"
        <| fun _ ->
            let expected = 2
            let actual = flip (fun a _ -> a) 1 2
            Expect.equal actual expected null

        testCase "trim removes leading and trailing"
        <| fun _ ->
            let expected = "a"
            let actual = trim " a "
            Expect.equal actual expected null

        testCase "before can be happy"
        <| fun _ ->
            let expected = "hello"
            let actual = "hello world" |> before " "
            Expect.equal actual expected null

        testCase <| "after can be happy"
        <| fun _ ->
            let expected = "world"
            let actual = "hello world" |> after " "
            Expect.equal actual expected null

        testCase <| "chunkBy can be happy"
        <| fun _ ->
            let expected = [[2];[2;3];[2;3;3];[4;5];[6]]
            let actual = [ 2;2;3;2;3;3;4;5;6] |> chunkBy (fun x -> x % 2 = 0)
            Expect.equal actual expected null
    ]

[<Tests>]
let setTests =
    testList "set module" [
        testCase "addAll can be happy"
        <| fun _ ->
            let expected = Set[1;2;3;4]
            let actual = Set[1] |> Set.addAll [2;3;4]
            Expect.equal actual expected null
    ]

[<Tests>]
let mapTests =
    testList "map module" [
        testList "addItem" [
            testCase "can add new key"
            <| fun _ ->
                let expected = Map[1,[2]]
                let actual = Map.empty |> Map.addItem 1 2
                Expect.equal actual expected null

            testCase "can add to existing key"
            <| fun _ ->
                let expected = Map[1,[3;2]]
                let actual = Map[1,[2]] |> Map.addItem 1 3
                Expect.equal actual expected null
        ]

        testCase "mergeAsList"
        <| fun _ ->
            let toAdd = Map[1,3]
            let expected = Map[1,[3;2]]
            let actual = Map[1,[2]] |> Map.mergeAsList toAdd
            Expect.equal actual expected null
    ]

[<Tests>]
let asyncTests =
    testList "async module" [
        testCase "can be happy"
        <| fun _ ->
            let expected = 5
            let actual = async { return 1} |> Async.map(fun x -> x + 4) |> Async.RunSynchronously
            Expect.equal actual expected null
    ]


[<Tests>]
let optionBuilderTests =
    testList "optionBuilder module" [
        testCase "can be Some happy"
        <| fun _ ->
            let expected = Some 5
            let actual =
                option{
                    return 5
                }
            Expect.equal actual expected null
        testCase "can be None happy"
        <| fun _ ->
            let expected = None
            let actual =
                option{
                    return! None
                }
            Expect.equal actual expected null
    ]

