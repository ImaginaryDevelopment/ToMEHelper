/// Functionality for posting character vault queries and scraping the results
namespace ToMEHelper.Scraping.CharacterVault

open ToMEHelper.BHelpers

module Logging =
    let mutable logger : ToMELogger option = None
    let inline dump (x:'t) =
        logger
        |> Option.iter(fun logger ->
            logger.Dump(x)
        )
    let inline dumpt title x =
        logger
        |> Option.iter(fun logger ->
            logger.Dump(x,description=title)
        )

module Defaults =
    let authority = "https://te4.org/"

module VaultScraping =
    open ToMEHelper.Schema
    open ToMEHelper.Scraping.ParseHelpers

    module Defaults =
        let path = "characters-vault"

    let rowToCharacter (x:HtmlAgilityPack.HtmlNode) =
        x
        |> getElements "td"
        |> List.ofSeq
        |> function // columns aren't always consistent
            //| userTd::charTd::_ -> (userTd,charTd)
            | userTd::charTd::_ -> Some (userTd,charTd)
            | td :: [] when (getText td).Contains("No characters available") -> None
            | bad ->
                bad |> List.map getOuterHtml |> Logging.dumpt "character row failure"
                failwithf "unexpected number of elements %i in %s" bad.Length (bad |> List.map getOuterHtml |> String.concat "")
        |> Option.map (fun (userTd,charTd) ->
                let getHrefWithInner x =
                    x
                    |> getElement "a"
                    |> Option.map (getHrefWithInner>>Tuple2.mapFst (Option.defaultValue null))
                    |> Option.defaultValue (null,null)
                let _userhref,uname = userTd |> getHrefWithInner
                let cHref,cName = charTd |> getHrefWithInner
                Logging.dump(cName,cHref)
                let value: CharacterLinkRaw = {User=uname;Name=cName;Path=cHref} // ;Link=null}
                value
        )

    /// expects a document parsed from the result of submitting a query to the character vault
    let getCharacters (doc:HtmlAgilityPack.HtmlDocument) =
        getElementById "characters" doc
        |> getElement "table"
        |> Option.bind (getElement "tbody")
        |> Option.map (getElements "tr" >> List.choose rowToCharacter)
        |> Option.defaultValue List.empty


