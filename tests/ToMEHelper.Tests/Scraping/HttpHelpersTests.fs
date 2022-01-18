
module ToMEHelper.Scraping.HttpHelpersTests

open System
open Expecto
open TestHelpers

open ToMEHelper.Scraping.HttpHelpers
open System.Net.Http

[<Tests>]
let uncategorizedTests = testList "HttpHelpers" [
    testList "HtmlMsgHandler" [
        let sut f = new HtmlMsgHandler(f)
        testCase "can construct"
        <| fun _ ->
            sut ignore
            |> ignore
            ()
        testCase "sendAsync calls f before sending"
        <| fun _ ->
            let msg = "hello world"
            let f _ =
                invalidOp msg
            let x = sut f
            let req = new HttpRequestMessage()
            req.RequestUri <- Uri("http://localhost")
            let cts = new System.Threading.CancellationTokenSource()
            let token = cts.Token
            Expect.throwsC(fun () ->
                x.SendAsync' (req, token)
                |> Async.AwaitTask
                |> Async.RunSynchronously
                |> ignore
            ) (
                function
                | :? InvalidOperationException as iex ->
                    iex
                | ex -> failwithf "Ex message was %s" ex.Message
            )
            |> fun actual ->
                Expect.equal actual.Message msg null
            ()


    ]
]