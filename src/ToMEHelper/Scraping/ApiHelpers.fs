module ToMEHelper.Scraping.ApiHelpers
open System

open ToMEHelper.BHelpers

open ToMEHelper.Schema

type ApiPair = {
    Id: int
    Key: Guid
}

let getClassFromId =
    function
    | 104 -> Ok Adventurer
    | 19 -> Ok Alchemist
    | 326744 -> Ok Annihilator
    | 20 -> Ok Anorithil
    | 22 -> Ok ArcaneBlade
    | 14 -> Ok Archer
    | 7 -> Ok Archmage
    | 16 -> Ok Berserker
    | 56 -> Ok Brawler
    | 80 -> Ok Bulwark
    | 34 -> Ok Corruptor
    | 133921 -> Ok CultistOfEntropy
    | 10 -> Ok Cursed
    | 23297 -> Ok Demonologist
    | 23313 -> Ok Doombringer
    | 29 -> Ok Doomed
//  | 267 -> Ok Dwarf
//  | 103389 -> Ok Ghoul
    | 208 -> Ok Gunslinger
//  | 10201 -> Ok Halfling
//  | 31036 -> Ok Higher
//  | 815099 -> Ok Insane
    | 71 -> Ok Marauder
    | 48 -> Ok Mindslayer
    | 68 -> Ok Necromancer
//  | 322125 -> Ok Ogre
    | 179 -> Ok Oozemancer
//  | 94927 -> Ok Orc
    | 43 -> Ok ParadoxMage
    | 95691 -> Ok Possessor
    | 67509 -> Ok Psyshot
    | 31 -> Ok Reaver
    | 12 -> Ok Rogue
    | 67403 -> Ok Sawbutcher
    | 23 -> Ok Shadowblade
//  | 284 -> Ok Shalore
//  | 341234 -> Ok Skeleton
    | 12400 -> Ok Skirmisher
    | 102 -> Ok Solipsist
    | 70 -> Ok StoneWarden
    | 17 -> Ok Summoner
    | 27 -> Ok SunPaladin
    | 49 -> Ok TemporalWarden
    | 104071 -> Ok WrithingOne
    | 4 -> Ok Wyrmic
    | x -> Error x

let getCampaignFromId =
    function
    | 2 -> Ok Maj
    | 46 -> Ok Arena
    | 24 -> Ok Infinite
    | 67402 -> Ok Orcs
    | x -> Error x

let getDifficultyFromId =
    function
    | 33 -> Ok Easy
    | 6 -> Ok Normal
    | 26 -> Ok Nightmare
    | 227 -> Ok Madness
    | 36 -> Ok Insane
    | x -> Error x

let getPermadeathFromId =
    function
    | 72 -> Ok Exploration
    | 65 -> Ok Adventure
    | 66 -> Ok Roguelike
    | x -> Error x

// let getRaceFromId =
//     function


type ApiResultRaw = {
    uname: string
    id_version: int
    id_profile: int
    id_campaign: int
    status: string
    id_race: int
    uuid: System.Guid
    level: int
    id_class: int
    winner: string
    title: string
    id_permadeath: int
    official_addons: string
    charsheet_api: string
    last_updated: string
    id_difficulty: int
}
    with
        static member ToApiResult (x:ApiResultRaw) = {
            UserName= x.uname
            Version= x.id_version
            ProfileId= x.id_profile
            Campaign= getCampaignFromId x.id_campaign
            Alive= x.status = "alive"
            Race= Error x.id_race // getRaceFromId x.id_race
            Id= x.uuid
            Level= x.level
            Class= getClassFromId x.id_class
            Winner= x.winner = "yes"
            Title= x.title
            Permadeath= getPermadeathFromId x.id_permadeath
            OfficialAddons= x.official_addons = "yes"
            CharsheetApi= x.charsheet_api
            LastUpdated= x.last_updated
            Difficulty= getDifficultyFromId x.id_difficulty
        }

let baseUrl = "http://zigur.te4.org"

let getCharPath apiPair characterId =
    match characterId.Owner with
    | OnlineId i ->
        sprintf "/%i/%A/characters/get/%i/tome/%A/json&online_id=1" apiPair.Id apiPair.Key i characterId.Id
    | OwnerId i ->
        sprintf "/%i/%A/characters/get/%i/tome/%A/json" apiPair.Id apiPair.Key i characterId.Id

type RawMapType= System.Collections.Generic.Dictionary<string,int>
// http://zigur.te4.org/#api_vault_get_filters
// the raw result of asking the api what filters are available
type CharacterFilterMeta = {
    id_version: RawMapType
    id_race: RawMapType
    id_difficulty: RawMapType
    winner: string[]
}

// not currently supporting custom classes/difficulties/etc
type CharacterFilter = {
    Permadeath: Permadeath option
    Difficulty: Difficulty option
    Winner: bool option
    Race: ToMERace option
    Status: obj option
    Class: ToMEClass option
    Campaign: Campaign option
    LevelMin: int option
    LevelMax: int option
    Versions: string list
    OnlyOfficialAddons: bool option
}

// type ApiResultRaw with

//     static member FromRaw (x:ApiResult) =
//         {

//         }

let getCharacterFindPath apiPair characterFilter =
    let pairs = Map [
        match characterFilter.Permadeath with
        | None -> ()
        | Some pd ->
            yield "id_permadeath",
            match pd with
            | Exploration -> 2
            | Roguelike -> 3
            | Adventure -> 1
            |> string
    ]
    // "/:api_id/:api_key/characters/find"
    let path = sprintf "%i/%A/characters/find" apiPair.Id apiPair.Key
    HttpHelpers.buildQuery pairs path

let deserializeApiResults (x:string) =
    System.Text.Json.JsonSerializer.Deserialize<ApiResultRaw[]>(json=x)
