namespace ToMEHelper.Schema

type ToMERace =
    | Cornac
    | Doomelf
    | Drem
    | Dwarf
    | Ghoul
    | Halfling
    | Higher
    | Krog
    | KrukYeti
    | Lich
    | Ogre
    | Orc
    | Shalore
    | Skeleton
    | Thalore
    | Whitehoof
    | Yeek
    member x.GetDisplay() =
        match x with
        | KrukYeti -> "Kruk Yeti"
        | _ -> string x
    // for LINQPad
    member private x.ToDump() = x.GetDisplay()

type ToMEClass =
    | Adventurer
    | Alchemist
    | Annihilator
    | Anorithil
    | ArcaneBlade
    | Archer
    | Archmage
    | Berserker
    | Brawler
    | Bulwark
    | Corruptor
    | CultistOfEntropy
    | Cursed
    | Demonologist
    | Doombringer
    | Doomed
    // | Dwarf
    // | Ghoul
    | Gunslinger
    // | Halfling
    // | Higher
    // | Insane
    | Marauder
    | Mindslayer
    | Necromancer
    // | Ogre
    | Oozemancer
    // | Orc
    | ParadoxMage
    | Possessor
    | Psyshot
    | Reaver
    | Rogue
    | Sawbutcher
    | Shadowblade
    // | Shalore
    // | Skeleton
    | Skirmisher
    | Solipsist
    | StoneWarden
    | Summoner
    | SunPaladin
    | TemporalWarden
    | WrithingOne
    | Wyrmic
    // | Yeek
    // for LINQPad
    member private x.ToDump() = x.GetDisplay()
    member x.GetDisplay() =
        match x with
        | ArcaneBlade -> "Arcane Blade"
        | CultistOfEntropy -> "Cultist Of Entropy"
        | ParadoxMage -> "Paradox Mage"
        | StoneWarden -> "Stone Warden"
        | SunPaladin -> "Sun Paladin"
        | TemporalWarden -> "Temporal Warden"
        | _ -> string x

type Difficulty =
    | Easy
    | Normal
    | Nightmare
    | Madness
    | Insane
    // for LINQPad
    member private x.ToDump() = string x

type OwnerType =
    | OnlineId of int
    // api ownerId
    | OwnerId of int

/// note owner id is different on website vs api
type CharacterId = { Owner: OwnerType; Id: System.Guid }


[<NoComparison>]
type CharacterLinkRaw =
    { User: string
      Name: string
      Path: string } // ;Link:obj}

type CharacterLink =
    { User: string
      Name: string
      Path: string
      Level: int option
      Race: ToMERace option
      Class: ToMEClass option }

type Campaign =
    | Arena
    | Infinite
    | Maj
    | Orcs
    // for LINQPad
    member private x.ToDump() = string x

type Permadeath =
    | Adventure // small # of lives
    | Exploration
    | Roguelike // 1 life
    // for LINQPad
    member private x.ToDump() = string x


type ApiResult =
    { UserName: string
      Version: int
      ProfileId: int
      Campaign: Result<Campaign, int>
      Alive: bool
      Race: Result<ToMERace, int>
      Id: System.Guid
      Level: int
      // Custom ints or unmapped can be errors
      Class: Result<ToMEClass, int>
      Winner: bool
      Title: string
      Permadeath: Result<Permadeath, int>
      OfficialAddons: bool
      CharsheetApi: string
      LastUpdated: string
      Difficulty: Result<Difficulty, int> }
      with
        static member TryValidate (x:ApiResult) : ValidatedApiResult option =
            match x.Campaign, x.Race, x.Class, x.Permadeath, x.Difficulty with
            | Ok cam, Ok r, Ok cls, Ok pd, Ok diff ->
                Some {
                    UserName = x.UserName
                    Version = x.Version
                    ProfileId = x.ProfileId
                    Campaign = cam
                    Alive = x.Alive
                    Race = r
                    Id = x.Id
                    Level = x.Level
                    Class = cls
                    Winner = x.Winner
                    Title = x.Title
                    Permadeath = pd
                    OfficialAddons = x.OfficialAddons
                    CharsheetApi = x.CharsheetApi
                    LastUpdated = x.LastUpdated
                    Difficulty = diff
                }
            | _ -> None

and ValidatedApiResult =
    { UserName: string
      Version: int
      ProfileId: int
      Campaign: Campaign
      Alive: bool
      Race: ToMERace
      Id: System.Guid
      Level: int
      Class: ToMEClass
      Winner: bool
      Title: string
      Permadeath: Permadeath
      OfficialAddons: bool
      CharsheetApi: string
      LastUpdated: string
      Difficulty: Difficulty }
and ApiResultType =
    | Valid of ValidatedApiResult
    | Raw of ApiResult

module Charsheets =
    type Prodigies = Map<string, int> // assuming prodigies only go 1/1 for now

    type StatSummary =
        { StatName: string
          Base: int
          Effective: int }
        with
            member private x.ToDump() =
                sprintf "%s: %i (base %i)" x.StatName x.Effective x.Base

    type StatCharsheet = StatSummary list

    type CharacterCharsheet =
        { Campaign: string
          Mode: string * string
          Race: string
          Class: string }

type Talent =
    { Name: string
      Points: int }
    // with member x.Dump() = sprintf "%s-%i" x.Name x.Points
    member x.ToDump() = sprintf "%s-%i" x.Name x.Points

type TalentCategory =
    { Name: string
      Power: string
      Talents: Talent list }

type TalentCategoryType =
    | Class
    | Generic
    member x.ToDump() = string x

type TalentPower =
    | TalentPower of string
    member x.ToDump() =
        match x with
        | TalentPower v -> v

module Aggregation =
    type CategoryInvestment =
        | CategoryInvestment of int
        member x.ToDump() =
            match x with
            | CategoryInvestment v -> v

    type CategoryAnaly =
        { TotalPoints: int
          TalentsObtained: int }

    [<NoComparison; NoEquality>]
    type MappedCharacter =
        { Character: CharacterLink
          Prodigies: Charsheets.Prodigies
          ClassTalents: TalentCategory list
          GenericTalents: TalentCategory list }

    type SequenceStatData =
        { Mean: decimal
          Median: int
          Min: int
          Max: int
          Population: int }
