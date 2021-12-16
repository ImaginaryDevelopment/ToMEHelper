module TestHelpers

open Expecto
open ToMEHelper.BHelpers
open ToMEHelper.Scraping.ParseHelpers


// // run a test with a timeout
// // we have a test that seems to be non-terminating
// let runWithTimeout name timeout f =
//     testCase name
//     <| fun _ ->
//         async {
//             return f()
//         }
//         |> Async.withTimeout timeout
//         |> Async.RunSynchronously
let htmlNodesOfText (x:string) =
    sprintf "<html><body><div id=\"node\">%s</div></body></html>" x |> parseHtml |> getElementById "node" |> getChildren