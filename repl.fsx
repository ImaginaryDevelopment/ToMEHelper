// explore vault, increase REPL of code changes
#r "nuget: HTMLAgilityPack"
#load "./src/ToMEHelper/BHelpers.fs"
#load "./src/ToMEHelper/Schema.fs"
    // <Compile Include="Scraping\ScrapeHelpers.fs" />
    // <Compile Include="Scraping\ParseHelpers.fs" />
    // <Compile Include="Scraping\Characters.fs" />
    // <Compile Include="Scraping\CharacterVault.fs" />
#load "./src/ToMEHelper/Scraping/ScrapeHelpers.fs"
#load "./src/ToMEHelper/Scraping/ParseHelpers.fs"
#load "./src/ToMEHelper/Scraping/Characters.fs"
#load "./src/ToMEHelper/Scraping/CharacterVault.fs"

open ToMEHelper.BHelpers
open ToMEHelper.Schema

module Scraping =
    open ToMEHelper.Scraping.ScrapeHelpers
    open ToMEHelper.Scraping.ParseHelpers
    open ToMEHelper.Scraping.CharacterVault.VaultScraping
    let pdOpt = [Permadeath.Roguelike ] |> List.map (getPermadeathId >> string) |> Some
    let diffOpt = None
    let alwaysInputs =
        Map [
            "tag_official_addons", ["1"] // only querying characters using only official addons
            "tag_campaign[]", [Campaign.Maj |> ToMEHelper.Scraping.ScrapeHelpers.getCampaignId |> string] // only regular maj
            //"tag_permadeath[]", [ Permadeath.Roguelike.FormValue |> snd]
            //"tag_difficulty[]", [Difficulty.Normal.FormValue  |> snd]
            match pdOpt with
            | Some values -> "tag_permadeath[]", values
            | None -> ()
            match diffOpt with
            | Some values -> "tag_difficulty[]", values |> List.map snd
            | None -> ()
            "tag_winner", ["winner"] // only query winners
            //"tag_level_min", ["50"]
            //"tag_dead",["dead"] // I query dead ppl
        ]
    let queryVault m pgOpt =
        let m =
            match pgOpt with
            | None -> m
            | Some pg -> m |> Map.add "page" (string<int> pg)
            |> flip Map.mergeAsList  alwaysInputs
        printfn "%A" m
        let url = sprintf "%s%s" ToMEHelper.Scraping.CharacterVault.Defaults.authority Defaults.path
        ToMEHelper.Scraping.ScrapeHelpers.Fetch.queryPage' 2 url m
        |> Async.map(Result.map( parseHtml >> getCharacters >> List.map (fun x -> x, clFromRaw x)))

Scraping.queryVault Map.empty (Some 5)
|> Async.RunSynchronously
|> printfn "%A"