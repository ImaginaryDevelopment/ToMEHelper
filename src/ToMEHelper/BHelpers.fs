module ToMEHelper.BHelpers

type ToMELogger =
    abstract Dump<'t> : 't -> unit
    abstract Dump<'t> : 't * description: string -> unit

let (|ValueString|Whitespace|EmptyString|NullString|) =
    function
    | null -> NullString
    | "" -> EmptyString
    | x when System.String.IsNullOrWhiteSpace x -> Whitespace x
    | x -> ValueString x


let flip f x y = f y x
let trim (x: string) = x.Trim()

let failNonValue name =
    function
    | ValueString _ -> ()
    | _ -> failwithf "Required a value for %s" name

let failNullOrEmpty name =
    function
    | ValueString _ -> ()
    | ""
    | null -> failwithf "Required a value or whitespace for %s" name
    | _ -> ()

// not intended to work on splitting whitespace on whitespace
let split delim =
    failNullOrEmpty "delim" delim

    function
    | Whitespace x
    | ValueString x -> x.Split(delim)
    | _ -> Array.empty

let tryBefore delim =
    failNullOrEmpty "delim" delim

    fun (x: string) ->
        match x.IndexOf delim with
        | i when i >= 0 -> Some x.[0..i - 1]
        | _ -> None

let tryAfter delim =
    failNullOrEmpty "delim" delim

    fun (x: string) ->
        match x.IndexOf delim with
        | i when i >= 0 -> Some x.[i + delim.Length..]
        | _ -> None

let before (delim: string) (x: string) = x.[0..x.IndexOf delim - 1]
let after (delim: string) (x: string) = x.[x.IndexOf delim + delim.Length..]

// f -> true starts a new bucket
let chunkBy f x =
    let rec loop chunk chunks list =
        match list with
        | [] -> List.rev ((List.rev chunk) :: chunks)
        | x :: xs when f x && List.isEmpty chunk -> loop [ x ] chunks xs
        | x :: xs when f x -> loop [ x ] ((List.rev chunk) :: chunks) xs
        | x :: xs -> loop (x :: chunk) chunks xs

    loop [] [] x

module StringHelpers =
    let equalsI (y: string) (x: string) =
        System.String.Equals(x, y, System.StringComparison.InvariantCultureIgnoreCase)

    let tryParse f (x: string) =
        match f x with
        | true, v -> Some v
        | _ -> None

    let (|Before|_|) delim = tryBefore delim
    let (|After|_|) delim = tryAfter delim
    let (|Int|_|) = tryParse System.Int32.TryParse

    let tryParseDU<'t> (x: string) =
        FSharp.Reflection.FSharpType.GetUnionCases(typeof<'t>)
        |> Array.tryFind (fun uc -> equalsI uc.Name x)
        |> Option.map (fun uc -> FSharp.Reflection.FSharpValue.MakeUnion(uc, Array.empty) :?> 't)




module Option =
    let ofValueString =
        function
        | ValueString x -> Some x
        | _ -> None

[<RequireQualifiedAccess>]
module Tuple2 =
    let mapFst f (x, y) = f x, y
    let mapSnd f (x, y) = x, f y
    let replicate x = x, x
    let curry f x y = f (x, y)
    let uncurry f (x, y) = f x y
    let swap (x, y) = y, x

    // ascending pair
    let asc (x, y) = min x y, max x y
    // descending pair
    let desc (x, y) = max x y, min x y

    // checked addition would throw, we want (x,x+1)
    let ofIncr x =
        if x = System.Int32.MaxValue then
            None
        else
            Some(x, x + 1)
    // checked subtraction would throw, we want (x, x-1)
    let ofDecr x =
        if x = System.Int32.MinValue then
            None
        else
            Some(x, x - 1)

    let optionOfPair (x, y) =
        match (x, y) with
        | Some x, Some y -> Some(x, y)
        | _ -> None

    let optionOfFst f (x, y) =
        match f x with
        | Some x -> Some(x, y)
        | None -> None

    let optionOfSnd f (x, y) =
        match f y with
        | Some y -> Some(x, y)
        | None -> None


    let iter (f1: _ -> unit) (f2: _ -> unit) (x, y) =
        f1 x
        f2 y

    module Helpers =
        // works with anything that supports comparison
        let (|CompAsc|CompEq|CompDesc|) (x, y) =
            if x < y then CompAsc(x, y)
            elif x > y then CompDesc(x, y)
            else CompEq x

        let (|OptBoth|OptLeft|OptRight|OptNeither|) =
            function
            | None, None -> OptNeither
            | Some x, Some y -> OptBoth(x, y)
            | Some x, None -> OptLeft x
            | None, Some y -> OptRight y

module Set =
    let addAll items s =
        (s, items) ||> Seq.fold (fun s v -> Set.add v s)

module Result =
    let partition items =
        ((List.empty, List.empty), items)
        ||> List.fold (fun (good, bad) ->
            function
            | Ok x -> x :: good, bad
            | Error e -> good, e :: bad)

module Map =
    let addItem k x (m: Map<_, 't list>) =
        if m.ContainsKey k then
            let existing = m.[k]
            m |> Map.add k (x :: existing)
        else
            m |> Map.add k [ x ]

    // merge the data in m2 into m1, using m2's values as additional items in m1's list
    let mergeAsList (m2: Map<'tk, 'tv>) (m: Map<'tk, 'tv list>) : Map<'tk, 'tv list> =
        (m, m2)
        ||> Map.fold (fun m k v -> m |> addItem k v)

module Async =
    open System.Threading
    let result x = async { return x }

    let map f x =
        async {
            let! x2 = x
            return f x2
        }
    // https://stackoverflow.com/questions/22245268/f-async-workflow-with-timeout
    let withTimeout timeout x =
        async {
            let! child = Async.StartChild(x, timeout)

            try
                let! result = child
                return Ok result
            with
            | :? System.TimeoutException -> return Error "timeout"
        }
    // let withCancellation (token:CancellationToken) x =
    //     async{
    //         try
    //             let! value = Async.Start(x, cancellationToken = token)
    //             return Ok value
    //         with
    //             | :? TaskCanceledException  as ex -> return Error ex
    //             | :? AggregateException as ex -> return Error ex
    //     }
    // let withTokenTimeout timeout x =
    //     async {
    //         use tokenSource = new CancellationTokenSource (timeout)
    //         return! x |> Async
    //     }


    let retry retries f x =
        let rec retry i (exs: exn list) =
            async {
                try
                    let! value = f x
                    return Ok value
                with
                | ex ->
                    if i > 0 then
                        return! retry (i - 1) <| ex :: exs
                    else
                        return Error(ex :: exs)
            }

        retry retries List.empty

    let retryBind retries f x =
        let rec retry i (exs: exn list) =
            async {
                try
                    match! f x with
                    | Ok value -> return Ok value
                    | Error ex -> return! retry (i - 1) <| ex :: exs
                with
                | ex ->
                    if i > 0 then
                        return! retry (i - 1) <| ex :: exs
                    else
                        return Error(ex :: exs)
            }

        retry retries List.empty

type Tree<'t> = { Value: 't; Children: Tree<'t> list }

module Tree =
    let rec map f { Value = x; Children = trees } =
        { Value = f x
          Children = trees |> List.map (map f) }

    let leaf x = { Value = x; Children = List.empty }
    // untested and unused
    let rec tryFind f root =
        if f root.Value then
            Some root
        else
            root.Children
            |> Seq.choose (tryFind f)
            |> Seq.tryHead

    let rec count { Value = _; Children = children } : int =
        children |> List.map count |> List.sum |> (+) 1

    let rec generateFromTree (ind, i) f x =
        [ yield
            f x.Value
            |> sprintf "%s%s" (String.replicate i ind)
          yield!
              x.Children
              |> List.collect (generateFromTree (ind, i + 1) f) ]

    let generateFromForest (ind, i) f items =
        items
        |> List.collect (generateFromTree (ind, i) f)

[<Struct>]
type OptionalBuilder =
    member __.Bind(opt, binder) =
        match opt with
        | Some value -> binder value
        | None -> None

    member __.Return(value) = Some value

    member __.ReturnFrom x =
        match x with
        | Some (Some x) -> Some x
        | _ -> None

let option = OptionalBuilder()
