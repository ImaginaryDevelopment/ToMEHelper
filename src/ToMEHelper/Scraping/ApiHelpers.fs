module ToMEHelper.Scraping.ApiHelpers

open System

open ToMEHelper.BHelpers

open ToMEHelper.Schema
open ToMEHelper.Scraping.SiteSchema

type ApiPair = { Id: int; Key: Guid }


type ApiResultRaw =
    { uname: string
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
      id_difficulty: int }
    static member ToApiResult(x: ApiResultRaw) =
        { UserName = x.uname
          Version = x.id_version
          ProfileId = x.id_profile
          Campaign = getCampaignFromId x.id_campaign
          Alive = x.status = "alive"
          Race = getRaceFromId x.id_race
          Id = x.uuid
          Level = x.level
          Class = getClassFromId x.id_class
          Winner = x.winner = "yes"
          Title = x.title
          Permadeath = getPermadeathFromId x.id_permadeath
          OfficialAddons = x.official_addons = "yes"
          CharsheetApi = x.charsheet_api
          LastUpdated = x.last_updated
          Difficulty = getDifficultyFromId x.id_difficulty }

let baseUrl = "http://zigur.te4.org"

let getCharPath apiPair characterId =
    match characterId.Owner with
    | OnlineId i -> sprintf "/%i/%A/characters/get/%i/tome/%A/json?online_id=1" apiPair.Id apiPair.Key i characterId.Id
    | OwnerId i -> sprintf "/%i/%A/characters/get/%i/tome/%A/json" apiPair.Id apiPair.Key i characterId.Id

// type RawMapType = System.Collections.Generic.Dictionary<string, int>
// http://zigur.te4.org/#api_vault_get_filters
// the raw result of asking the api what filters are available
// type CharacterFilterMeta =
//     { id_version: RawMapType
//       id_race: RawMapType
//       id_difficulty: RawMapType
//       winner: string [] }

[<RequireQualifiedAccess>]
type CharacterFilterField =
    | Permadeath
    | Difficulty
    | Winner
    | Race
    | Alive
    | Class
    | Campaign
    | LevelMin
    | LevelMax
    | Versions
    | OnlyOfficialAddons
    with
        static member All = [
            CharacterFilterField.Permadeath
            CharacterFilterField.Difficulty
            CharacterFilterField.Winner
            CharacterFilterField.Race
            CharacterFilterField.Alive
            CharacterFilterField.Class
            CharacterFilterField.Campaign
            CharacterFilterField.LevelMin
            CharacterFilterField.LevelMax
            CharacterFilterField.Versions
            CharacterFilterField.OnlyOfficialAddons
        ]

// not currently supporting custom classes/difficulties/etc
type CharacterFilter =
    { Permadeath: Permadeath option
      Difficulty: Difficulty option
      Winner: bool option
      Race: ToMERace option
      Alive: bool option
      Class: ToMEClass option
      Campaign: Campaign option
      LevelMin: int option
      LevelMax: int option
      Versions: string list
      OnlyOfficialAddons: bool option }
      with
        static member Empty =
            {
                Permadeath = None
                Difficulty = None
                Winner = None
                Race = None
                Alive = None
                Class = None
                Campaign = None
                LevelMin = None
                LevelMax = None
                Versions = List.empty
                OnlyOfficialAddons = None
            }

let getCharacterFilterFieldName =
    function
    | CharacterFilterField.Permadeath -> "id_permadeath"
    | CharacterFilterField.Difficulty -> "id_difficulty"
    | CharacterFilterField.Winner -> "winner"
    | CharacterFilterField.Race -> "id_race"
    | CharacterFilterField.Alive -> "status"
    | CharacterFilterField.Class -> "id_class"
    | CharacterFilterField.Campaign -> "id_campaign"
    | CharacterFilterField.LevelMin -> "level_min"
    | CharacterFilterField.LevelMax -> "level_max"
    | CharacterFilterField.Versions -> "id_version"
    | CharacterFilterField.OnlyOfficialAddons -> "only_official_addons"

module Meta =
    type Prop<'t> = CharacterFilterField * (CharacterFilter -> 't) * ('t -> CharacterFilter -> CharacterFilter)
    let PermadeathProp: Prop<_> =
        CharacterFilterField.Permadeath, (fun x -> x.Permadeath), fun y x -> {x with Permadeath = y}
    let DifficultyProp: Prop<_> =
        CharacterFilterField.Difficulty, (fun x -> x.Difficulty), fun y x -> {x with Difficulty = y}
    let WinnerProp: Prop<_> =
        CharacterFilterField.Winner, (fun x -> x.Winner), fun y x -> {x with Winner = y}
    let RaceProp: Prop<_> =
        CharacterFilterField.Race, (fun x -> x.Race), fun y x -> {x with Race = y}
    let AliveProp: Prop<_> =
        CharacterFilterField.Alive, (fun x -> x.Alive), fun y x -> {x with Alive = y}
    let ClassProp: Prop<_> =
        CharacterFilterField.Class, (fun x -> x.Class), fun y x -> {x with Class = y}
    let CampaignProp: Prop<_> =
        CharacterFilterField.Campaign, (fun x -> x.Campaign), fun y x -> {x with Campaign = y}
    let LevelMinProp: Prop<_> =
        CharacterFilterField.LevelMin, (fun x -> x.LevelMin), fun y x -> {x with LevelMin = y}
    let LevelMaxProp: Prop<_> =
        CharacterFilterField.LevelMax, (fun x -> x.LevelMax), fun y x -> {x with LevelMax = y}
    let VersionsProp: Prop<_> =
        CharacterFilterField.Versions, (fun x -> x.Versions), fun y x -> {x with Versions = y}
    let OnlyOfficialAddonsProp: Prop<_> =
        CharacterFilterField.OnlyOfficialAddons, (fun x -> x.OnlyOfficialAddons), fun y x -> {x with OnlyOfficialAddons = y}
    let getPropGetter ((_,g,_):Prop<_>) = g
    let getPropSetter((_,_,s): Prop<_>) = s

let toStringMap (fGet: CharacterFilterField -> CharacterFilter -> (string list) option) (filter:CharacterFilter) =
    CharacterFilterField.All
    |> Seq.choose (fun f ->
        option {
            let! v = fGet f filter
            let! fn = getCharacterFilterFieldName f |> Option.ofValueString
            return (fn,v)
        }

    )

let getStringValues (field:CharacterFilterField) (x:CharacterFilter): string list option =
    match field with
    | CharacterFilterField.Permadeath -> x.Permadeath |> Option.map (getPermadeathId>>string>> List.singleton)
    | CharacterFilterField.Difficulty -> x.Difficulty |> Option.map (getDifficultyId>>string >> List.singleton)
    | CharacterFilterField.Winner -> x.Winner |> Option.map (fun v -> (if v then "yes" else "no") |> List.singleton)
    | CharacterFilterField.Race -> x.Race |> Option.map(getRaceId>>string >> List.singleton)
    | CharacterFilterField.Alive -> x.Alive |> Option.map((function | true -> "alive" | _ -> "dead") >> List.singleton) // skipping for now
    | CharacterFilterField.Class -> x.Class |> Option.map(getClassId>>string >> List.singleton)
    | CharacterFilterField.Campaign -> x.Campaign |> Option.map(getCampaignId>>string >> List.singleton)
    | CharacterFilterField.LevelMin -> x.LevelMin |> Option.map (string >> List.singleton)
    | CharacterFilterField.LevelMax -> x.LevelMax |> Option.map (string >> List.singleton)
    | CharacterFilterField.Versions -> match x.Versions |> List.choose (getVersionId) with | [] -> None | x -> x |> List.map string |> Some
    | CharacterFilterField.OnlyOfficialAddons -> x.OnlyOfficialAddons |> Option.map((function | true -> "yes" | _ -> "no")>>List.singleton)

let getCharacterFindPath apiPair characterFilter =
    let pairs =
        toStringMap getStringValues characterFilter
        |> Map.ofSeq

    // "/:api_id/:api_key/characters/find"
    let path =
        sprintf "%i/%A/characters/find" apiPair.Id apiPair.Key

    HttpHelpers.buildQuery pairs path

let deserializeApiResults (x: string) =
    System.Text.Json.JsonSerializer.Deserialize<ApiResultRaw []>(json = x)
