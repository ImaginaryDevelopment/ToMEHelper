module ToMEHelper.BHelpersTests

open System
open Expecto
// open Expecto.ExpectoFsCheck
open FsCheck

open TestHelpers

open ToMEHelper
open ToMEHelper.BHelpers

let failsBadDelim f =
            testList "bad delim throws" [
                testCase "null"
                <| fun _ -> Expect.throws(fun () -> f null |> ignore) null
                testCase "empty"
                <| fun _ -> Expect.throws(fun () -> f "" |> ignore) null
            ]
[<Tests>]
let uncategorizedTests =
    testList "uncategorized" [
        testList "ValueStrings" [
            testList "ValueString" [
                let vs x = match x with | ValueString x -> Some x | _ -> None

                testCase "null -> None"
                <| fun _ ->
                    let actual = vs null
                    Expect.isNone actual null

                testCase "'' -> None"
                <| fun _ ->
                    let actual = vs ""
                    Expect.isNone actual null

                testCase "' ' -> None"
                <| fun _ ->
                    let actual = vs " "
                    Expect.isNone actual null

                testCase "\\t -> None"
                <| fun _ ->
                    let actual = vs "\t"
                    Expect.isNone actual null

                testCase "'a' -> Some 'a'"
                <| fun _ ->
                    let expected = Some "a"
                    let actual = vs "a"
                    Expect.equal actual expected null
            ]
            testCase "NullString can be happy"
            <| fun _ ->
                match null with
                | NullString -> ()
                | x -> failwithf "Null produced %A" x
            testCase "EmptyString can be happy"
            <| fun _ ->
                match "" with
                | EmptyString -> ()
                | x -> failwithf "Empty produced %A" x
            testList "Whitespace" [
                testCase "' ' can be happy"
                <| fun _ ->
                    match " " with
                    | Whitespace x ->
                        Expect.equal x " " null
                    | x -> failwithf "Whitespace did not match for '%s'" x
            ]
        ]
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



        testList "failNonValue" [
            testProperty "values are values" (fun (x:string) ->
                match x with
                | null | "" -> Expect.throws (fun () -> failNonValue "x" x) null
                | x when String.IsNullOrWhiteSpace x -> Expect.throws (fun () -> failNonValue "x" x) null
                | x -> failNonValue "x" x
            )
            testList "throw literals" (
                [ "null",null; "empty",""; "space"," "; "tab","\t" ; "ret","\r"; "newline", "\n"]
                |> List.map(fun (n,v) -> testCase n (fun _ -> Expect.throws (fun () -> failNonValue n v) null))
            )
        ]
        testList "failNullOrEmpty" [
            testProperty "values are values" (fun (x:string) ->
                match x with
                | null | "" -> Expect.throws (fun () -> failNonValue "x" x) null
                | x -> failNullOrEmpty "x" x
            )
            testList "throw literals" (
                [ "null",null; "empty",""]
                |> List.map(fun (n,v) -> testCase n (fun _ -> Expect.throws (fun () -> failNonValue n v) null))
            )
        ]
        testList "split" [
            failsBadDelim split
            testList "happy" [
                testCase "spaces"
                <| fun _ ->
                    let expected = [| "hello"; "world" |]
                    let actual = "hello world" |> split " "
                    Expect.equal actual expected null
            ]
        ]
        testList "tryBefore" [
            failsBadDelim tryBefore
            testCase "can be happy"
            <| fun _ ->
                let actual = tryBefore " " "hello world"
                Expect.equal actual (Some "hello") null
            testCase "can be None"
            <| fun _ ->
                let actual = tryBefore " " "hello"
                Expect.isNone actual null
        ]
        testList "tryAfter" [
            failsBadDelim tryAfter
            testCase "can be happy"
            <| fun _ ->
                let actual = tryAfter " " "hello world"
                Expect.equal actual (Some "world") null
            testCase "can be None"
            <| fun _ ->
                let actual = tryAfter " " "hello"
                Expect.isNone actual null
        ]

        testCase <| "chunkBy can be happy"
        <| fun _ ->
            let expected = [[2];[2;3];[2;3;3];[4;5];[6]]
            let actual = [ 2;2;3;2;3;3;4;5;6] |> chunkBy (fun x -> x % 2 = 0)
            Expect.equal actual expected null
    ]

[<Tests>]
let optionModuleTests = testList "Option" [
        testCase "ofValueString can be happy"
        <| fun _ ->
            "" |> Option.ofValueString |> Assert.equal None
            "a" |> Option.ofValueString |> Assert.equal (Some "a")
]


module StringHelpers =
    open StringHelpers
    [<Tests>]
    let stringHelpersTests = testList "stringHelpers" [
        testList "tryParse" [
            let tryParseOfOption f x =
                tryParse (fun x -> match f x with | None -> false,Unchecked.defaultof<_> | Some x -> true,x) x
            testCase "can be happy"
            <| fun _ ->
                let text = "hello world"
                let expected = tryBefore " " text
                let actual = tryParse (function | Before " " x -> true,x | _ -> false,null) text
                Expect.equal actual expected null
            testCase "can be none"
            <| fun _ ->
                let text = "helloworld"
                let expected = tryBefore " " text
                let actual = tryParseOfOption (tryBefore " ") text
                Expect.equal actual expected null
        ]
        testList "After" [
            testCase "can be none"
            <| fun _ ->
                let text = "helloworld"
                match text with | After " " _ -> failwith "there was no space" | _ -> ()
            testCase "can be happy"
            <| fun _ ->
                let text = "hello world"
                match text with  | After " " x -> Expect.equal x "world" null | _ -> failwith "after did not match"
        ]
        testList "Int" [
            testCase "can be none"
            <| fun _ ->
                let text = ""
                match text with | Int i -> failwith "that was not an int" | _ -> ()
            testCase "can be happy"
            <| fun _ ->
                let expected = 1
                let text = string expected
                match text with | Int actual -> Expect.equal actual expected null | _ -> failwith "that was an int"
            testProperty "any int can be happy"
            <| fun (expected:int) ->
                let text = string expected
                match text with | Int actual -> Expect.equal actual expected null | _ -> failwith "that was an int"
        ]
    ]
// // module Tuple2Tests =
// [<Tests>]
// let tuple2Tests = testList "Tuple2" [
//     testCase "mapFst can be happy"
//     <| fun _ -> (1,2) |> Tuple2.mapFst ((+)1) |> Assert.equal (2,2)
//     testCase "mapSnd can be happy"
//     <| fun _ -> (2,1) |> Tuple2.mapSnd ((+)1) |> Assert.equal (2,2)

// ]
[<Tests>]
let tuple2Tests =
    testList "Tuple2" [
        testCase "mapFst can be happy"
        <| fun _ ->
            let expected = (2,1)
            let actual = (1,1) |> Tuple2.mapFst ((+) 1)
            Expect.equal actual expected null
        testCase "mapSnd can be happy"
        <| fun _ ->
            let expected = (1,2)
            let actual = (1,1) |> Tuple2.mapSnd ((+) 1)
            Expect.equal actual expected null
        testProperty "replicate int"
        <| fun (i:int) ->
            Tuple2.replicate i
            |> Assert.equal (i,i)
        testProperty "curry"
        <| fun (x:int,y:int) ->
            Tuple2.curry (fun (x,y) -> x + y) x y
            |> Assert.equal (x + y)
        testProperty "uncurry"
        <| fun (x:int,y:int) ->
            Tuple2.uncurry (fun x y -> x + y) (x,y)
            |> Assert.equal (x + y)
        testProperty "swap"
        <| fun (x:int,y:int) ->
            Tuple2.swap (x,y)
            |> Assert.equal(y,x)

        testProperty "asc"
        <| fun (x:int,y:int) ->
            let l,u = Tuple2.asc (x,y)
            Expect.isAscending [l;u] null
        testProperty "desc"
        <| fun (x:int,y:int) ->
            let u,l = Tuple2.desc(x,y)
            Expect.isDescending [u;l]
        testProperty "ofIncr"
        <| fun x ->
            let actual = Tuple2.ofIncr x
            if System.Int32.MaxValue = x then
                Expect.isNone actual null
            else actual |> Assert.equal (Some (x,x+1))
        testProperty "ofDecr"
        <| fun x ->
            let actual = Tuple2.ofDecr x
            if System.Int32.MinValue = x then
                Expect.isNone actual null
            else actual |> Assert.equal (Some (x, x-1))

        testList "optionOfFst" [
            testCase "can be happy"
            <| fun _ ->
                let expected = Some(1,2)
                Tuple2.optionOfFst Some (1,2)
                |> Assert.equal expected
            testCase "can be none"
            <| fun _ ->
                Tuple2.optionOfFst id (None,1)
                |> Assert.equal None
        ]
        testList "optionOfSnd" [
            testCase "can be happy"
            <| fun _ ->
                let expected = Some(1,2)
                Tuple2.optionOfSnd Some (1,2)
                |> Assert.equal expected
            testCase "can be none"
            <| fun _ ->
                Tuple2.optionOfSnd id (1,None)
                |> Assert.equal None
        ]
    ]

[<Tests>]
let resultTests=
    testList "Result" [
        testList "partition" [
            testCase "works on empty elements"
            <| fun _ ->
                let expected = List.empty,List.empty
                let actual = List.empty |> Result.partition
                Expect.equal actual expected null
            testCase "works with only Oks"
            <| fun _ ->
                let v = 2
                let expected = [2], List.empty
                let actual = [Ok v] |> Result.partition
                Expect.equal actual expected null
            testCase "works with only Errors"
            <| fun _ ->
                let v = "bad"
                let expected = List.empty, [v]
                let actual = [Error v] |> Result.partition
                Expect.equal actual expected null
            testCase "works with equal amounts"
            <| fun _ ->
                let okv, ev = 1,"error"
                let expected = [okv], [ev]
                let actual = [Error ev; Ok okv] |> Result.partition
                Expect.equal actual expected null
        ]
    ]
[<Tests>]
let setTests =
    testList "Set" [
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
        testCase "result can be happy"
        <| fun _ ->
            let expected = 5
            let actual = Async.result 5 |> Async.RunSynchronously
            Expect.equal actual expected null
        testCase "map can be happy"
        <| fun _ ->
            let expected = 5
            let actual = async { return 1} |> Async.map(fun x -> x + 4) |> Async.RunSynchronously
            Expect.equal actual expected null
        testList "withTimeout" [
            testCase "can be happy"
            <| fun _ ->
                let expected = Ok 5
                let actual = async {return 5} |> Async.withTimeout 1 |> Async.RunSynchronously
                Expect.equal actual expected null

            testCase "can timeout"
            <| fun _ ->
                async {
                    do! Async.Sleep 6000
                    return 5
                 }
                 |> Async.withTimeout 1
                 |> Async.RunSynchronously
                 |> function
                    | Ok x -> failwithf "Expected timeout, got %A instead" x
                    | Error _ -> ()
        ]

        testList "retry" [
            testCase "can be happy"
            <| fun _ ->
                let expected = 5
                let actual = Async.retry 0 (fun () -> async { return expected}) () |> Async.RunSynchronously
                match actual with
                | Ok actual ->
                    Expect.equal actual expected null
                | Error _ ->
                    failwith "Errors in happy path"
            testCase "does retry"
            <| fun _ ->
                let expected = 5
                let mutable i = -1
                let actual =
                    Async.retry 1
                        (fun () ->
                            i <- i + 1
                            async { if i = 0 then return failwith "I'm a failure" else return expected }
                        ) ()
                    |> Async.RunSynchronously
                Expect.equal actual (Ok expected) null
        ]
        testList "retryBind" [
            testCase "can be happy"
            <| fun _ ->
                let expected = 5
                let actual = Async.retryBind 0 (fun () -> async { return Ok expected}) () |> Async.RunSynchronously
                match actual with
                | Ok actual ->
                    Expect.equal actual expected null
                | Error _ ->
                    failwith "Errors in happy path"
            testCase "does retry"
            <| fun _ ->
                let expected = 5
                let mutable i = -1
                let actual =
                    Async.retryBind 1
                        (fun () ->
                            i <- i + 1
                            async { if i = 0 then return failwith "I'm a failure" else return Ok expected }
                        ) ()
                    |> Async.RunSynchronously
                Expect.equal actual (Ok expected) null
        ]
    ]

[<Tests>]
let treeTests = testList "Tree" [
    testList "map" [
        testCase "identity"
        <| fun _ ->
            let expected = Tree.leaf 1
            expected
            |> Tree.map id
            |> Assert.equal expected
        testCase "addition"
        <| fun _ ->
            let expected = {Value=2;Children=[Tree.leaf 3; Tree.leaf 4]}
            {Value=1;Children = [Tree.leaf 2;Tree.leaf 3]}
            |> Tree.map ((+) 1)
            |> fun x -> x
            |> Assert.equal expected
    ]

    testCase "leaf"
    <| fun _ ->
        let expected = {Value=2;Children=List.empty}
        Tree.leaf 2
        |> Assert.equal expected

    testList "tryFind" [
        testCase "notFound"
        <| fun _ ->
            Tree.leaf 2
            |> Tree.tryFind(fun x -> x = 3)
            |> flip Expect.isNone null
        let testCanFind name target tree =
            testCase name
            <| fun _ ->
                tree
                |> Tree.tryFind(fun x -> x = target.Value)
                |> Assert.equal (Some target)
        testCanFind "root" (Tree.leaf 2) (Tree.leaf 2)
        testCanFind "first child"(Tree.leaf 2) {Value=1;Children=[Tree.leaf 2]}
        testCanFind "can find in secondary child"(Tree.leaf 2) {Value=4;Children=[Tree.leaf 3;Tree.leaf 2]}
    ]

    testList "count" [
        let testTreeLength name expected tree =
            testCase name
            <| fun _ ->
                tree
                |> Tree.count
                |> Assert.equal expected
        testTreeLength "single" 1 (Tree.leaf 1)
        testProperty "non-parents" (fun i -> {Value=0;Children = List.init (abs i) Tree.leaf} |> Tree.count |> Assert.equal (abs i + 1))
        testTreeLength "peers" 3 {Value=2;Children=[Tree.leaf 2; Tree.leaf 2]}
    ]
    let treeCases = [
        "happy"," ",0, string, Tree.leaf 2, ["2"]
        "can handle children"," ",0,string,Tree.leaf 2,["2"]
        "indented children", "  ",1,string,{Value=2;Children=[Tree.leaf 3]}, ["  2";"    3"]
        "tabbed children", "\t",1,string,{Value=2;Children=[Tree.leaf 3]}, ["\t2";"\t\t3"]
        "sibling children", " ",0,string,{Value=2;Children=[Tree.leaf 3;Tree.leaf 4]},["2"; " 3"; " 4"]
    ]
    testList "generateFromTree" [
        // testCase "can be happy"
        // <| fun _ ->
        //     Tree.generateFromTree (" ",0) string (Tree.leaf 2)
        //     |> Assert.equal [ "2" ]
        // testCase "can handle children"
        // <| fun _ ->
        //     Tree.generateFromTree (" ",0) string ({Value=2;Children=[Tree.leaf 3]})
        //     |> Assert.equal ["2";" 3"]
        // testCase "can handle indented children"
        // <| fun _ ->
        //     Tree.generateFromTree (" ",1) string ({Value=2;Children=[Tree.leaf 3]})
        //     |> Assert.equal [" 2";"  3"]
        // testCase "can handle sibling children"
        // <| fun _ ->
        //     Tree.generateFromTree(" ",0) string ({Value=2;Children=[Tree.leaf 3;Tree.leaf 4]})
        //     |> Assert.equal ["2"; " 3"; " 4"]
        yield! treeCases |> List.map(fun (name,ind,i,f,tree,expected) ->
            testCase name
            <| fun _ ->
                Tree.generateFromTree (ind,i) string tree
                |> Assert.equal expected
        )
    ]
    testList "generateFromForest" [
        yield! treeCases |> List.map(fun (name,ind,i,f,tree,expected) ->
            testCase name
            <| fun _ ->
                Tree.generateFromForest (ind,i) string [tree]
                |> Assert.equal expected
        )
    ]
]
module TextParsing =
    open TextParsing
    open Tuple2.Helpers

    [<Tests>]
    let textParsing = testList "TextParsing" [
        let empty = {Index=0;Indent=None;Line=null}
        testList "getIndentation" [
            testCase "can be happy"
            <| fun _ ->
                " a"
                |> getIndentation ' '
                |> Assert.equal (Some 1)
        ]
        testList "addNode" [
            testProperty "can have a child"
            <| fun (i,ind1,value1,ind2,value2) ->
                let ind1,ind2 =
                    let ind1' = min ind1 ind2
                    let v = max ind1 ind2
                    ind1',if v = ind1 then ind1 + 1 else v
                Expect.isLessThanOrEqual ind1 ind2 null
                let parent ={Index=i;Indent=Some ind1;Line=value1}
                let child = {Index=i;Indent=Some ind2;Line=value2}
                let expected = {Value=parent;Children=[Tree.leaf child]}
                addNode [Tree.leaf parent] child
                |> Assert.equal [expected]

            testProperty "handles any combo"
            <| fun (ind1Opt:PositiveInt option,ind2Opt: PositiveInt option) ->
                let parent = {empty with Indent = ind1Opt |> Option.map(fun x -> x.Get)}
                let child = {empty with Indent = ind2Opt |> Option.map(fun x -> x.Get)}
                match ind1Opt, ind2Opt with
                | OptBoth (CompAsc _) -> // will be a child
                    addNode [Tree.leaf parent] child
                    |> Assert.equal [{Tree.leaf parent with Children=[Tree.leaf child]}]
                | OptBoth (CompDesc _) -> // will be invalid tree
                    Assert.throws
                    <| fun _ ->
                        addNode [Tree.leaf parent] child
                        |> ignore
                | OptBoth (CompEq _) -> // will be siblings
                    addNode [Tree.leaf parent] child
                    |> Assert.equal [Tree.leaf child; Tree.leaf parent]
                | OptRight _ -> // parent is nonvalue
                    Assert.throws
                    <| fun _ ->
                        addNode [Tree.leaf parent] child
                        |> ignore
                | OptLeft _ -> // will be empty child
                    addNode [Tree.leaf parent] child
                    |> Assert.equal [{Value=parent;Children=[Tree.leaf child]}]
                | OptNeither -> // siblings?
                    addNode [Tree.leaf parent] child
                    |> Assert.equal [Tree.leaf child;Tree.leaf parent]

            testProperty "can fail a child" // should always fail when next node is less indented than current
            <| fun (PositiveInt ind1) ->
                match Tuple2.ofDecr ind1 with
                | Some(pi,nextI) ->
                    Expect.isLessThan nextI pi "decr failure?"
                    let parent = {Index=0;Indent = Some pi;Line=null}
                    let child = {Index=0;Indent = Some nextI;Line=null}
                    Expect.throws(fun () -> addNode [Tree.leaf parent] child |> ignore<Tree<_> list>) (sprintf "(%i,%i)" pi nextI)
                | None ->
                    Expect.throws(fun () -> addNode [Tree.leaf {empty with Indent = None}] {empty with Indent= Some 0} |> ignore<Tree<_> list>) null

            testProperty "can make a sibling"
            <| fun (i,ind,value1,value2) ->
                let parent = {Index=i;Indent=Some ind;Line=value1}
                let sib = {Index=i;Indent=Some ind;Line=value2}
                let expected = [Tree.leaf sib;Tree.leaf parent]
                addNode [Tree.leaf parent] sib
                |> Assert.equal expected

        ]
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

