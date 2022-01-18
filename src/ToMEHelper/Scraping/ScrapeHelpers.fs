module ToMEHelper.Scraping.ScrapeHelpers

open ToMEHelper.BHelpers

open ToMEHelper.Schema

// let getRaceId =
//     function
//     | ToMERace.Cornac ->



let tryGetRace tokens =
    (None,[0.. List.length tokens])
    ||> List.fold(fun r i ->
        r
        |> Option.orElseWith (fun () ->
            tokens.[0..i]
            |> String.concat ""
            |> StringHelpers.tryParseDU<ToMERace>
            |> Option.map(fun r -> r, tokens.[(i+1)..])
        )
    )
let tryGetClass tokens =
    (None, [0.. List.length tokens])
    ||> List.fold(fun (r: (_*_) option) i ->
        r
        |> Option.orElseWith(fun () ->
            tokens.[0..i]
            |> String.concat ""
            |> StringHelpers.tryParseDU<ToMEClass>
            |> Option.map(fun r -> r, tokens.[(i+1)..])
        )
    )

module Fetch =
    module Internals =
        type HttpClientType =
            | GlobalInternal
            | CustomClient of System.Net.Http.HttpClient
            | Custom of (System.Uri -> Async<string>)

        let hc = lazy(new System.Net.Http.HttpClient())

        let getPage hct (path:System.Uri) =
            let f () =
                match hct with
                | GlobalInternal -> hc.Value.GetStringAsync path |> Async.AwaitTask
                | CustomClient hc -> hc.GetStringAsync path |> Async.AwaitTask
                | Custom f -> f path
            async{
                try
                    let! text = f ()
                    return Ok text
                with ex ->
                    return Error ex
            }

        // take a potentially multiple value query key and build the string
        let toQueryValues k items =
            items
            |> Seq.map(System.Uri.EscapeDataString >> sprintf "%s=%s" k)

        let queryPage' retries hct path kvs =
            if path |> String.exists ((=) '?') |> not then
                let query = (List.empty,kvs) ||> Map.fold(fun s k v -> v |> toQueryValues k |> String.concat "&" |> fun v -> v :: s) |> String.concat "&"
                let fullpath = sprintf "%s?%s" path query |> System.Uri
                let result = Async.retryBind retries (getPage hct) fullpath
                result
            else
                invalidOp "bad path" |> List.singleton |> Error |> Async.result

    open Internals

    let inline queryPage' retries path kvs =
        queryPage' retries GlobalInternal path kvs




