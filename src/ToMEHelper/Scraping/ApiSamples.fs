/// Opinionated Module for accessing the api in specific ways
module ToMEHelper.Scraping.ApiSamples

open System.Linq

open ToMEHelper.Schema
open ToMEHelper.Scraping.ApiHelpers
open ToMEHelper.BHelpers
open ToMEHelper.BHelpers.StringHelpers

module Helpers =
    module Cereal =
        let jsonOpts = System.Text.Json.JsonSerializerOptions(PropertyNameCaseInsensitive = true)
        let inline deserialize<'t>(x:string) =
            System.Text.Json.JsonSerializer.Deserialize<'t>(x,jsonOpts)
        let inline deserializeEle(x:string) =
            System.Text.Json.JsonSerializer.Deserialize<System.Text.Json.JsonElement>(x, jsonOpts)
        let deserializeForPath (propName:string) x =
            let je = deserializeEle x
            je.GetProperty(propName) |> string

    // consider foldback
    let partitionResults items =
        ((List.empty,List.empty),items)
        ||> Seq.fold(fun (okies,errs) item ->
            match item with
            | Ok v -> v::okies, errs
            | Error e -> okies, e::errs
        )
        |> fun (o,e) -> List.rev o, List.rev e
    ()

open Helpers
open Helpers.Cereal

module HttpClient =
    open System.Net.Http
    let getStringAsync (hc:HttpClient) (path:string) =
        hc.GetStringAsync(path)
let getHC baseUrl fLogger apiPair =
    let hc =  ToMEHelper.Scraping.HttpHelpers.httpClient fLogger
    hc.BaseAddress <- baseUrl
    hc.DefaultRequestHeaders.Add("x-api-id", string apiPair.Id)
    hc.DefaultRequestHeaders.Add("x-api-key", string apiPair.Key)
    hc

let getFilters httpclient =
    let path = "characters/get_filters"
    HttpClient.getStringAsync httpclient path

let getProfileIdFromOnlineId httpclient onlineId =
    let path = getProfileIdFromOnlinePath onlineId
    HttpClient.getStringAsync httpclient path

let getChar httpclient characterId =
    let path = getCharPath characterId
    HttpClient.getStringAsync httpclient path

let findChars httpclient characterFilter pagingOpt =
    let path = getCharacterFindPath characterFilter pagingOpt
    HttpClient.getStringAsync httpclient path

// handling automatic page walking
let findCharsAllPages httpclient characterFilter =
    Seq.initInfinite(fun i ->
        match i with
        | 0 -> None
        | i -> Some {
            Page = Some i
            Max = None
            Order = null
        }
        |> findChars httpclient characterFilter
    )

let dumpFindCharsValidation httpclient (logger:ToMELogger) extraDebug characterFilter pageOpt =
    findChars httpclient characterFilter pageOpt
    |> Async.AwaitTask
    |> Async.RunSynchronously
    |> fun x ->
        if extraDebug then logger.Dump(x) |> ignore
        x
    |> deserializeApiResults
    |> Result.map(fun rs->
        rs
        |> Seq.map (ApiResultRaw.ToApiResult >> fun x -> x |> ApiResult.TryValidate |> Option.map Ok |> Option.defaultValue (Error x))
        |> List.ofSeq
        |> fun x ->
            if extraDebug then logger.Dump(x) |> ignore
            x
        |> Result.foldIfAll
        |> logger.Dump
        |> ignore
    )

let getRaceData httpclient (logger:ToMELogger) =
    getFilters httpclient
    |> Async.AwaitTask
    |> Async.RunSynchronously
    |> System.Text.Json.JsonSerializer.Deserialize<CharacterFilterMeta>
    |> fun x -> x.id_race.AsEnumerable()
    |> Seq.map (|KeyValue|)
    |> Seq.map(fun (k,v) ->
        ToMEHelper.BHelpers.StringHelpers.tryParseDU<ToMERace>(k)
        |> Option.map (fun r -> Ok(r,v))
        |> Option.defaultValue (Error (k,v)))
    |> partitionResults
    |> fun (o,e) ->
        o
        |> Seq.map(fun (r,i) ->

            sprintf "    | %i -> Ok %A" i r
        )
        |> String.concat "\r\n"
        |> logger.Dump
        |> ignore
        o
        |> Seq.map (snd>>string)
        |> String.concat ";"
        |> logger.Dump
        |> ignore
        logger.Dump(o,"ok")
        logger.Dump(e,"errs")

let fetchValidChar httpclient (x:ValidatedApiResult) =
    HttpClient.getStringAsync httpclient x.CharsheetApi

let (|JsonExPath|_|) =
    function
    // example: Path: $['primary stats'].willpower.value
    // assuming only 1 [''] prop
    | After "ToMEHelper.Scraping.Characters.RawApiCharacters" (After ". Path: $['" (Before " | Line" v)) ->
        v.Split(".")
        |> Array.mapi(fun i x ->
            if i = 0 then
                x |> before "']"
            else x
        )
        |> Some
    | After "ToMEHelper.Scraping.Characters.RawApiCharacters" (After ". Path: $." (Before " |" v)) ->
        v.Split(".")
        |> Some
    | _ -> None


let gatherDeserializeApiCharacterDiag (logger:ToMELogger) (x:string) (je:System.Text.Json.JsonElement) (ex:exn) =
    logger.Dump(ex,"could not deserialize")
    match ex.Message with
    | JsonExPath props ->
        let propPath = props |> String.concat "."
        (Ok je, props)
        ||> Seq.fold(fun state prop ->
            match state with
            | Ok je ->
                je.GetProperty(prop)
                |> Option.ofUnsafe
                |> function
                    | None -> Error je
                    | Some je -> Ok je
            | Error je -> Error je
        )
        |> function
            | Ok je2 -> logger.Dump(je2,sprintf "problem area: %s" propPath)
            | Error je2 -> logger.Dump(je2,sprintf "problem area- : %s" propPath)
    | _ ->
        printfn "Could not find error path"
    je

let tryDeserializeApiCharacter (logger:ToMELogger) (x:string) =
    // if it can't deserialize into an element, we have a bigger problem, do it here and now before try
    let je = deserializeEle x
    try
        ToMEHelper.Scraping.Characters.CharacterApi.deserializeApiCharacter id x
        |> Ok
    with ex ->
        gatherDeserializeApiCharacterDiag logger x je ex
        |> Error

let fetch1ValidCharDiag (logger:ToMELogger) httpclient chars =
    chars
    |> Seq.head
    |> fetchValidChar httpclient
    |> Async.AwaitTask
    |> Async.RunSynchronously
    |> tryDeserializeApiCharacter logger
    |> Result.mapError (fun je -> je.EnumerateObject())
