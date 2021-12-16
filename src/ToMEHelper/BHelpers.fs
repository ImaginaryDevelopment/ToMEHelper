module ToMEHelper.BHelpers

type ToMELogger =
    abstract Dump<'t> : 't -> unit
    abstract Dump<'t> : 't*description:string -> unit

let (|ValueString|_|) =
    function
    | null -> None
    | "" -> None
    | x when System.String.IsNullOrWhiteSpace x -> None
    | x -> Some x

let flip f x y = f y x
let trim (x:string) = x.Trim()
let before (delim:string) (x:string) = x.[0.. x.IndexOf delim - 1]
let after (delim:string) (x:string) = x.[x.IndexOf delim + delim.Length ..]

// f -> true starts a new bucket
let chunkBy f x =
    let rec loop chunk chunks list =
        match list with
        | [] -> List.rev ((List.rev chunk)::chunks)
        | x::xs when f x && List.isEmpty chunk -> loop [x] chunks xs
        | x::xs when f x -> loop [x] ((List.rev chunk)::chunks) xs
        | x::xs -> loop (x::chunk) chunks xs
    loop [] [] x

module Tuple2 =
    let mapFst f (x,y) = f x, y
    let mapSnd f (x,y) = x, f y

module Set =
    let addAll items s =
        (s,items) ||> Seq.fold(fun s v -> Set.add v s)

module Result =
    let partition items =
        ((List.empty,List.empty), items)
        ||> List.fold(fun (good,bad) ->
            function
            | Ok x -> x::good, bad
            | Error e -> good, e::bad
        )

module Map =
    let addItem k x (m:Map<_,'t list>) =
        if m.ContainsKey k then
            let existing = m.[k]
            m |> Map.add k (x::existing)
        else
            m |> Map.add k [x]

    // merge the data in m2 into m1, using m2's values as additional items in m1's list
    let mergeAsList (m2:Map<'tk,'tv >) (m:Map<'tk, 'tv list>) : Map<'tk, 'tv list> =
        (m,m2)
        ||> Map.fold(fun m k v ->
            m |> addItem k v
        )

module Async =
    open System.Threading
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
            with :? System.TimeoutException -> return Error "timeout"
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
        let rec retry i (exs:exn list) =
            async {
                try
                    let! value = f x
                    return Ok value
                with ex ->
                    if i > 0 then
                        return! retry (i - 1) <| ex::exs
                    else
                        return Error (ex::exs)
            }
        retry retries List.empty

    let retryBind retries f x =
        let rec retry i (exs:exn list) =
            async{
                try
                    match! f x with
                    | Ok value -> return Ok value
                    | Error ex -> return! retry (i-1) <| ex::exs
                with ex ->
                    if i > 0 then
                        return! retry (i - 1) <| ex::exs
                    else
                        return Error (ex::exs)
            }
        retry retries List.empty

[<Struct>]
type OptionalBuilder =
  member __.Bind(opt, binder) =
    match opt with
    | Some value -> binder value
    | None -> None
  member __.Return(value) =
    Some value
  member __.ReturnFrom x =
    match x with
    | Some (Some x) -> Some x
    | _ -> None

let option = OptionalBuilder()