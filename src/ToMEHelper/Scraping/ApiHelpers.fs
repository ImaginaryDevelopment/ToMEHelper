module ToMEHelper.Scraping.ApiHelpers
open System

open ToMEHelper.BHelpers

open ToMEHelper.Schema
open ToMEHelper.Scraping.SiteSchema

type ApiPair = {
    Id: int
    Key: Guid
}


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
            Race= getRaceFromId x.id_race
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
        sprintf "/%i/%A/characters/get/%i/tome/%A/json?online_id=1" apiPair.Id apiPair.Key i characterId.Id
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

let getCharacterFindPath apiPair characterFilter =
    let pairs = Map [
        match characterFilter.Permadeath with
        | None -> ()
        | Some pd ->
            yield "id_permadeath", getPermadeathId pd |> string
        match characterFilter.Difficulty with
        | None -> ()
        | Some d ->
            yield "id_difficulty", getDifficultyId d |> string
    ]
    // "/:api_id/:api_key/characters/find"
    let path = sprintf "%i/%A/characters/find" apiPair.Id apiPair.Key
    HttpHelpers.buildQuery pairs path

let deserializeApiResults (x:string) =
    System.Text.Json.JsonSerializer.Deserialize<ApiResultRaw[]>(json=x)
