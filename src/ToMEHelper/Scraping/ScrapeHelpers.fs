module ToMEHelper.Scraping.ScrapeHelpers

open ToMEHelper.BHelpers

open ToMEHelper.Schema

// let getRaceId =
//     function
//     | ToMERace.Cornac ->

let getClassId =
    function
    | Adventurer -> 104
    | Alchemist -> 19
    | Annihilator -> 326744
    | Anorithil -> 20
    | ArcaneBlade -> 22
    | Archer -> 14
    | Archmage -> 7
    | Berserker -> 16
    | Brawler -> 56
    | Bulwark -> 80
    | Corruptor -> 34
    | CultistOfEntropy -> 133921
    | Cursed -> 10
    | Demonologist -> 23297
    | Doombringer -> 23313
    | Doomed -> 29
    // | Dwarf -> 267
    // | Ghoul -> 103389
    | Gunslinger -> 208
    // | Halfling -> 10201
    // | Higher -> 31036
    // | Insane -> 815099
    | Marauder -> 71
    | Mindslayer -> 48
    | Necromancer -> 68
    // | Ogre -> 322125
    | Oozemancer -> 179
    // | Orc -> 94927
    | ParadoxMage -> 43
    | Possessor -> 95691
    | Psyshot -> 67509
    | Reaver -> 31
    | Rogue -> 12
    | Sawbutcher -> 67403
    | Shadowblade -> 23
    // | Shalore -> 284
    // | Skeleton -> 341234
    | Skirmisher -> 12400
    | Solipsist -> 102
    | StoneWarden -> 70
    | Summoner -> 17
    | SunPaladin -> 27
    | TemporalWarden -> 49
    | WrithingOne -> 104071
    | Wyrmic -> 4
    // | Yeek -> 170

let getDifficultyId =
    function
    | Easy -> 33
    | Normal -> 6
    | Nightmare -> 26
    | Madness -> 227
    | Insane -> 36

let getPermadeathId =
    function
    | Exploration -> 72
    | Adventure -> 65
    | Roguelike -> 66

let getCampaignId =
    function
    | Maj -> 2
    | Arena -> 46
    | Infinite -> 24
    | Orcs -> 67402

module Fetch =
    module Internals =
        type HttpClientType =
            | GlobalInternal
            | CustomClient of System.Net.Http.HttpClient
            | Custom of (System.Uri -> Async<string>)

        let hc = lazy(new System.Net.Http.HttpClient())

        let getPage hct (path:System.Uri) =
            let f () =
                match hct with
                | GlobalInternal -> hc.Value.GetStringAsync path |> Async.AwaitTask
                | CustomClient hc -> hc.GetStringAsync path |> Async.AwaitTask
                | Custom f -> f path
            async{
                try
                    let! text = f ()
                    return Ok text
                with ex ->
                    return Error ex
            }

        // take a potentially multiple value query key and build the string
        let toQueryValues k items =
            items
            |> Seq.map(System.Uri.EscapeDataString >> sprintf "%s=%s" k)

        let queryPage' retries hct path kvs =
            let query = (List.empty,kvs) ||> Map.fold(fun s k v -> v |> toQueryValues k |> String.concat "&" |> fun v -> v :: s) |> String.concat "&"
            let fullpath = sprintf "%s?%s" path query |> System.Uri
            Async.retryBind retries (getPage hct) fullpath

    open Internals

    let inline queryPage' retries path kvs =
        queryPage' retries GlobalInternal path kvs




