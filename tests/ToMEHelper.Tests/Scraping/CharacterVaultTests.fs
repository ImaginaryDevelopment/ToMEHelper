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
    testCase "dump can log"
    <| fun _ ->
        Logging.logger <- Some {
            new ToMEHelper.BHelpers.ToMELogger with
                member _.Dump(value) = value
                member _.Dump(_,_title) = ()
        }
        ToMEHelper.Scraping.CharacterVault.Logging.dump "hello"
        ToMEHelper.Scraping.CharacterVault.Logging.dumpt "title" "hello"
        ()
]
module VaultScrapingTests =
    open VaultScraping
    [<Tests>]
    let vaultScrapingTests = testList "VaultScraping" [
        let sample = """<tr class="odd"><td><a href="/user/295938/characters">Kiyoura</a></td><td><a href="/characters/295938/tome/d94a65b5-ee7c-4f1c-8b92-3af0d3297e66" style="color:#00FF00">Kiyoura the level 50 Higher Berserker</a></td><td>berserker</td><td>roguelike</td><td>maj'eyal</td><td>yes</td><td>yes</td><td>3 weeks 4 days ago</td> </tr>"""
        testList "rowToCharacter" [
            let doc = sprintf "<html><body><div id=\"node\">%s</div></body></html>" sample
            testCase "can be happy"
            <| fun _ ->
                let char = doc |> parseHtml |> getElementById "node" |> getElement "tr" |> Option.bind rowToCharacter
                match char with
                | Some char ->
                    Expect.equal char.User "Kiyoura" null
                    Expect.equal char.Path  "/characters/295938/tome/d94a65b5-ee7c-4f1c-8b92-3af0d3297e66" null
                    Expect.equal char.Name "Kiyoura the level 50 Higher Berserker" null
                | None -> failwith "unable to find tr"
        ]
        testList "clFromRaw" [
            testProperty "onlyNameMatters" (fun (u:string,p:string, i:int, r:ToMERace, c:ToMEClass) ->
                let n = "john"
                let input: CharacterLinkRaw = {User=u;Name=sprintf "%s the level %i %A %A" n i r c;Path=p}
                let expected:CharacterLink = {User=u;Name=n;Level=Some i;Path=p;Race=Some r;Class= Some c}
                let actual = clFromRaw input
                Expect.equal actual (Some expected) null
            )
            testCase "no name"
            <| fun _ ->
                let actual = clFromRaw {User = "john"; Name= ""; Path = ""}
                Expect.equal actual None null
            testCase "no level"
            <| fun _ ->
                let actual = clFromRaw {User = "john"; Name= "gozer the level "; Path = ""}
                Expect.equal actual None null
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