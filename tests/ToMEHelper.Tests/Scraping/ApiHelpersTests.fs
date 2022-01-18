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
        let apiId = 32514
        let charOwnerId = 31919
        testCase "can be happy" (fun _ ->
            let expected = sprintf "/32514/%A/characters/get/31919/tome/%s/json" apiPair.Key sampleCharGuid'
            let actual = getCharPath apiPair sampleChar
            Expect.equal actual expected null
        )
        testCase "can be happy with onlineId"
        <| fun _ ->
            let expected = sprintf "/32514/%A/characters/get/32514/tome/%s/json?online_id=1" apiPair.Key sampleCharGuid'
            let actual = getCharPath apiPair {sampleChar with Owner = OwnerType.OnlineId 32514}
            Expect.equal actual expected null
    ]
]