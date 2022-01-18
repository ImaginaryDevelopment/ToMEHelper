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
    testList "getCharPath" [
        let apiId = 32514
        let charOwnerId = 31919
        testCase "can be happy" (fun _ ->
            let expected = sprintf "/32514/%A/characters/get/31919/tome/%s/json" apiPair.Key sampleCharGuid'
            let actual = getCharPath apiPair sampleChar
            Expect.equal actual expected null
        )
    ]
]