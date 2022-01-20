module ToMEHelper.Scraping.ApiHelpersTests

open System
open Expecto
open TestHelpers

// open Expecto.ExpectoFsCheck
// open ToMEHelper
// open FSharp.Reflection
open ToMEHelper.Schema
open ToMEHelper.Scraping.ApiHelpers
open ToMEHelper.BHelpers
open ToMEHelper.Scraping.ScrapeHelpers
open ToMEHelper.Scraping.SiteSchema

type CharFilterAccess<'t> = CharacterFilter -> 't

[<Tests>]
let uncategorizedTests = testList "ApiHelpers" [
    let apiPair = {
        Id = 32514
        Key = Guid.NewGuid()
    }
    let sampleCharGuid' = "b5a2c97d-26f0-47a9-9296-23a70cc650d2"
    let sampleChar = {
        Owner = OwnerId 31919
        Id = Guid.Parse sampleCharGuid'
    }

    testList "ApiResultRaw" [
        testList "toApiResult" [
            let sut = ApiResultRaw.ToApiResult
            testCase "can be happy"
            <| fun _ ->
                let expectedCampaign = Campaign.Maj
                let expectedRace = Ok ToMERace.Halfling
                let expectedClass = Ok ToMEClass.Adventurer
                let expectedDiff = Ok Difficulty.Easy
                let expectedPDeath = Ok Permadeath.Roguelike
                let uuid = Guid.NewGuid()
                let raw =
                    {
                        uname = "uname"
                        id_version = 2
                        id_profile = 3
                        id_campaign = getCampaignId expectedCampaign
                        status = "alive"
                        id_race = 21 // halfling?
                        uuid = uuid
                        level = 4
                        id_class = getClassId ToMEClass.Adventurer
                        winner = "yes"
                        title = "hello kitty"
                        id_permadeath = getPermadeathId Permadeath.Roguelike
                        official_addons = "yes"
                        charsheet_api = null
                        last_updated = "65465465"
                        id_difficulty = getDifficultyId Difficulty.Easy
                    }
                raw
                |> sut
                |> fun x ->
                    Expect.equal x.UserName raw.uname "uname"
                    Expect.equal x.Version raw.id_version "id_version"
                    Expect.equal x.ProfileId raw.id_profile "id_profile"
                    Expect.equal x.Campaign (Ok expectedCampaign) "id_campaign"
                    Expect.equal x.Alive true "status"
                    Expect.equal x.Race expectedRace "id_race"
                    Expect.equal x.Id raw.uuid "uuid"
                    Expect.equal x.Level raw.level "level"
                    Expect.equal x.Class expectedClass "id_class"
                    Expect.equal x.Winner true "winner"
                    Expect.equal x.Title raw.title "title"
                    Expect.equal x.Permadeath expectedPDeath "id_permadeath"
                    Expect.equal x.OfficialAddons true "official_addons"
                    Expect.equal x.CharsheetApi raw.charsheet_api "charsheet_api"
                    Expect.equal x.LastUpdated raw.last_updated "last_updated"
                    Expect.equal x.Difficulty expectedDiff "id_difficulty"

        ]
    ]
    testList "getCharPath" [
        testCase "can be happy" (fun _ ->
            let expected = sprintf "characters/get/31919/tome/%s/json" sampleCharGuid'
            let actual = getCharPath sampleChar
            Expect.equal actual expected null
        )
        testCase "can be happy with onlineId"
        <| fun _ ->
            let expected = sprintf "characters/get/32514/tome/%s/json?online_id=1" sampleCharGuid'
            let actual = getCharPath {sampleChar with Owner = OwnerType.OnlineId 32514}
            Expect.equal actual expected null
    ]
    testList "Meta" [
        // yield! CharacterFilterField.All
        //     |> List.map(
        //             function
        //             | CharacterFilterField.Permadeath -> Meta.PermadeathProp |> ignore
        //             | CharacterFilterField.

        //     )
    ]
    testList "getPageStringValues" [
        let sut = ToMEHelper.Scraping.ApiHelpers.getPageStringValues
        testCase "empty"
        <| fun _ ->
            let expected = Map.empty
            let actual =
                {   Max = None
                    Page = None
                    Order = null
                }
                |> sut

            Expect.equal actual expected null
        testCase "full"
        <| fun _ ->
            let expected = Map [
                "max", string 2
                "page", string 3
            ]
            let actual =
                {
                    Max = Some 2
                    Page = Some 3
                    Order = null
                }
                |> sut
            Expect.equal actual expected null
    ]
    testList "getCharacterFindPath" [
        let sut = getCharacterFindPath
        let empty = CharacterFilter.Empty
        let full = {
                Permadeath = Some Permadeath.Roguelike
                Difficulty = Some Difficulty.Easy
                Winner = Some true
                Race = Some ToMERace.Krog
                Alive = Some true
                Class = Some ToMEClass.Solipsist
                Campaign = Some Campaign.Maj
                LevelMin = Some 20
                LevelMax = Some 60
                Versions = [ "tome-1.7.4" ]
                OnlyOfficialAddons = Some true
                ProfileId = Some 31919
        }
        let inline charFilter (f : CharFilterAccess<_>) x = f x

        // account for None and Some?
        let inline buildFieldTest (n:CharacterFilterField) x _ =
            let actual = sut x None
            let fieldKey = n |> getCharacterFilterFieldName |> sprintf "%s="
            match getStringValues n x with
            | None ->
                let hasKey = actual.Contains fieldKey
                Expect.isTrue (not hasKey) null
            | Some _ ->
                Expect.stringContains actual fieldKey null

        testCase "happy path"
        <| fun _ ->
            let expected = "characters/find?"
            let actual = sut empty None
            Expect.equal actual expected null
            ()
        yield! CharacterFilterField.All
            |> List.collect(fun field ->
                [
                    testCase (sprintf "%A empty" field) (buildFieldTest field empty)
                    testCase (sprintf "%A Some" field) (buildFieldTest field full)
                ]
            )
    ]
]