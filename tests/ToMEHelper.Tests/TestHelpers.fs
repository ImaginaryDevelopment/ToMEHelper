module TestHelpers

open Expecto
open ToMEHelper.BHelpers
open ToMEHelper.Scraping.ParseHelpers

let htmlNodesOfText (x:string) =
    sprintf "<html><body><div id=\"node\">%s</div></body></html>" x |> parseHtml |> getElementById "node" |> getChildren

module Assert =
    let equal expected actual = Expect.equal actual expected null
    let throws f = Expect.throws f null
    let clamp (lower,upper) x =
        if x < lower then lower
        elif x > upper then upper
        else x