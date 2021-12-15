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
    with
        member x.GetDisplay() =
            match x with
            | KrukYeti -> "Kruk Yeti"
            | _ -> string x

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
    | Shalore
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
    with
        member x.GetDisplay() =
            match x with
            | ArcaneBlade ->        "Arcane Blade"
            | CultistOfEntropy ->   "Cultist Of Entropy"
            | ParadoxMage ->        "Paradox Mage"
            | StoneWarden ->        "Stone Warden"
            | SunPaladin ->         "Sun Paladin"
            | TemporalWarden ->     "Temporal Warden"
            | _ -> string x

type Difficulty =
    | Normal
    | Insane

[<NoComparison>]
type CharacterLink = {User:string;Name:string;Path:string} // ;Link:obj}

type Campaign =
    | Arena
    | Infinite
    | Maj
    | Orcs

type Permadeath =
    | Adventure // small # of lives
    | Exploration
    | Roguelike // 1 life

module Charsheets =
    type Prodigies = Map<string,int> // assuming prodigies only go 1/1 for now
    type StatSummary = { StatName:string; Base:int; Effective:int}
    type StatCharsheet = StatSummary list
    type CharacterCharsheet = {Campaign:string;Mode:string*string;Race:string;Class:string}

type Talent = {Name:string;Points:int}
    // with member x.Dump() = sprintf "%s-%i" x.Name x.Points
    with member x.ToDump() = sprintf "%s-%i" x.Name x.Points

type TalentCategory = {Name:string; Power:string;Talents:Talent list}
type TalentCategoryType =
    | Class
    | Generic
    with member x.ToDump() = string x

type TalentPower = TalentPower of string with member x.ToDump() = match x with | TalentPower v -> v

module Aggregation =
    type CategoryInvestment = CategoryInvestment of int with member x.ToDump() = match x with | CategoryInvestment v -> v
    type CategoryAnaly = { TotalPoints: int; TalentsObtained:int}

    [<NoComparison;NoEquality>]
    type MappedCharacter = { Character:CharacterLink;Prodigies:Charsheets.Prodigies; ClassTalents: TalentCategory list;GenericTalents: TalentCategory list}

    type SequenceStatData = { Mean:decimal;Median:int; Min:int;Max:int;Population:int}
