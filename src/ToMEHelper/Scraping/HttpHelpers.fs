module ToMEHelper.Scraping.HttpHelpers

open System.Net.Http

open ToMEHelper.BHelpers

type HtmlMsgHandler(f) as me =
    inherit DelegatingHandler()
    do me.InnerHandler <- new HttpClientHandler()

    member x.SendAsync'(req, token) = x.SendAsync(req, token)

    override _.SendAsync(req, token) =
        match req.RequestUri.Query with
        | ValueString _ ->
            sprintf "Requesting %A" req.RequestUri |> f

            sprintf "Requesting query: %A" req.RequestUri.Query
            |> f

            base.SendAsync(req, token)
        | _ ->
            sprintf "Requesting %A" req.RequestUri |> f
            let resp = base.SendAsync(req, token)
            resp

let httpClient loggerOpt =
    match loggerOpt with
    | None -> new HttpClient()
    | Some f ->
        let mh = new HtmlMsgHandler(f)
        new HttpClient(mh, disposeHandler = true)

/// makes path + query
/// assumes no authority in path
let buildQuery queryMap (path: string) =
    let q =
        (List.empty, queryMap)
        ||> Map.fold (fun state k values ->
            let next =
                values |> List.map (System.Uri.EscapeDataString >> sprintf "%s=%s" k) |> String.concat "&"

            next :: state)
        |> String.concat "&"
    // account for a path that already has some query parts
    if path.Contains "?" then
        sprintf "%s&%s" path q
    else
        sprintf "%s?%s" path q
