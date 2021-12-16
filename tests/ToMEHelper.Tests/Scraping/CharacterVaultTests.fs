module ToMEHelper.Scraping.CharacterVaultTests

open System
open Expecto
open Expecto.ExpectoFsCheck
// open ToMEHelper
open FSharp.Reflection

open ToMEHelper.Schema
open ToMEHelper.Scraping.CharacterVault
open ToMEHelper.Scraping.ParseHelpers

[<Tests>]
let loggingTests = testList "CharacterVault Logging" [
    testCase "dump can be happy"
    <| fun _ ->
        ToMEHelper.Scraping.CharacterVault.Logging.dump "hello"
        |> ignore

    testCase "dumpt can be happy"
    <| fun _ ->
        ToMEHelper.Scraping.CharacterVault.Logging.dumpt "title" "hello"
]

[<Tests>]
let vaultScrapingTests = testList "VaultScraping" [
    let sample = """<tr class="odd"><td><a href="/user/295938/characters">Kiyoura</a></td><td><a href="/characters/295938/tome/d94a65b5-ee7c-4f1c-8b92-3af0d3297e66" style="color:#00FF00">Kiyoura the level 50 Higher Berserker</a></td><td>berserker</td><td>roguelike</td><td>maj'eyal</td><td>yes</td><td>yes</td><td>3 weeks 4 days ago</td> </tr>"""
    testList "rowToCharacter" [
        let doc = sprintf "<html><body><div id=\"node\">%s</div></body></html>" sample
        testCase "can be happy"
        <| fun _ ->
            let char = doc |> parseHtml |> getElementById "node" |> getElement "tr" |> Option.bind VaultScraping.rowToCharacter
            match char with
            | Some char ->
                Expect.equal char.User "Kiyoura" null
                Expect.equal char.Path  "/characters/295938/tome/d94a65b5-ee7c-4f1c-8b92-3af0d3297e66" null
                Expect.equal char.Name "Kiyoura the level 50 Higher Berserker" null
            | None -> failwith "unable to find tr"
    ]
    testList "getCharacters" [
        let doc = sprintf "<html><body><div id=\"node\"><div id=\"characters\"><table><tbody>%s</tbody></table></div></div></body></html>" sample
        testCase "can be happy"
        <| fun _ ->
            let chars = doc |> parseHtml |> VaultScraping.getCharacters
            match chars with
            | [ char ] ->
                Expect.equal char.User "Kiyoura" null
                Expect.equal char.Path  "/characters/295938/tome/d94a65b5-ee7c-4f1c-8b92-3af0d3297e66" null
                Expect.equal char.Name "Kiyoura the level 50 Higher Berserker" null
            | _ -> failwith "did not find expected number of characters"

    ]
]