/// Functionality for scraping character sheets
namespace ToMEHelper.Scraping.Characters

open ToMEHelper.BHelpers

type CDict<'tKey,'tValue> = System.Collections.Generic.Dictionary<'tKey, 'tValue>

module Logging =
    let mutable logger: ToMELogger option = None

    let inline dump (x: 't) =
        logger
        |> Option.iter (fun logger -> logger.Dump(x) |> ignore)

    let inline dumpt title x =
        logger
        |> Option.iter (fun logger -> logger.Dump(x, description = title))

open ToMEHelper.Scraping.ParseHelpers

module RawApiCharacters =
    type Inscription = {
        Name:string
        Desc:string
        Image:string
        Kind:string
    }
    type InscriptionStatus = {
        Used:string
        All: Inscription[]
    }
    type TalentX = {
        Name:string
        Desc:string
        Image:string
        Val:string
    }
    type TalentInfo = {
        Mastery: string
        List: TalentX[]
    }

    module OffenseTypes =
        type Mainhand = {
            crit: string
            APR: string
            damage: string
            accuracy: string
            speed: string
        }
        type Offhand = {
            crit: string
            penaly: float
            accuracy: string
            damage: string
            APR: string
            speed: string
        }

        type Mind = {
            mindpower: float
            crit: string
            speed: float
        }

        type Spell = {
            spellpower: float
            speed: float
            crit: string
            cooldown: float
        }

        type Offense = {
            mainhand: Mainhand[]
            offhand: Offhand[] // has penaly which will probably be renamed penalty eventually
            mind: Mind
            spell: Spell
            damage_pen: CDict<string,string>
            damage: CDict<string,string>
        }

    type InventoryItem = {
        Name: string
        Desc: string
        Image: string
    }
    module CharacterInfo =

        type Died = {
            times: float
            desc: string
            now: string
        }

        type Info = {
            permadeath: string
            difficulty: string
            size: string
            campaign: string
            Class: string
            sex: string
            race: string
            game: string
            addons: CDict<string,string>
            exp: string
            gold: string
            died: Died
            level: float
            name: string
            antimagic: bool
        }
    type Achievement = {
        Name: string
        Id: string
        Desc: string
        When: string
    }
    type Defense = {
        Resistances: CDict<string,string>
        Immunities: CDict<string,string>
        Defense: CDict<string,float>
    }
    type Stat = {
        Base:int
        // found a willpower value that was not a whole number
        Value:float
    }
    type CharacterSheet = {
        Inscriptions: InscriptionStatus
        Talents: CDict<string,TalentInfo>
        Version: string
        Offense: OffenseTypes.Offense
        Character: CharacterInfo.Info
        Resources: obj
        Effects: obj[]
        Vision: obj
        ``Primary Stats``: CDict<string,Stat>
        Achievements: Achievement[]
        Hidden: obj
        Sections: string[]
        Defense: Defense
        Healing: obj
        Last_Messages: obj
        Quests: obj[]
        Speeds: CDict<string,float>
        Winner: obj
        Equipment: CDict<string,obj[]>
        Inventory: InventoryItem[]
    }
module CharacterApi =
    open RawApiCharacters
    /// strips questionable/problematic api output issues before attempting
    let deserializeApiCharacter fSettings (x:string) : CharacterSheet =
        let jsonOpts =
            System.Text.Json.JsonSerializerOptions(PropertyNameCaseInsensitive = true)
            |> fSettings
        let f (x:string) = System.Text.Json.JsonSerializer.Deserialize<CharacterSheet>(json = x,options= jsonOpts)
        let replaceEmptyObj propName x =
            x
            |> replace (sprintf "\"%s\":{}" propName) (sprintf "\"%s\":[]" propName)
        x
        // |> replace "\"mainhand\":{}" "\"mainhand\":[]"
        |> replaceEmptyObj "mainhand"
        |> replaceEmptyObj "offhand"
        |> replaceEmptyObj "achievements"
        |> replaceEmptyObj "effects"
        |> replaceEmptyObj "quests"
        |> replaceEmptyObj "inventory"
        |> f

module Parsing =
    open ToMEHelper.Schema

    let parseTalent data spentTd =
        match data with
        | NodeName "td" (GetElement "ul" ul) ->
            let talentName =
                let last = getElements "li" ul |> Seq.last

                last
                |> getOuterHtml
                |> after "</div>"
                |> before "<"
                |> trim
            // what about talents that go higher or lower than 5?
            let value: Talent =
                { Name = talentName
                  Points = spentTd |> getText |> before "/5" |> int }

            Ok value
        | _ -> Error("nonmatch", data |> getOuterHtml)

    let mapTalents el : (TalentCategory * _) seq =
        match el |> getElement "table" with
        | None -> failwith "unable to find talent table"
        | Some x -> x
        |> getElements "tr"
        |> List.ofSeq
        |> chunkBy (function
            | GetElement "td" (GetElement "strong" _) -> true
            | _ -> false)
        |> Seq.choose (
            List.map getChildren
            >> (fun x ->
                let inline dumpNodes title =
                    x
                    |> List.map (List.map getOuterHtml)
                    |> Logging.dumpt title

                let (|StrongText|_|) =
                    function
                    | GetElement "strong" x -> x |> getText |> Some
                    | x ->
                        printfn "StrongText didn't match %A" (getOuterHtml x)
                        None

                let getTitle (nodes: _ list) =
                    match nodes with
                    | NodeName "td" (StrongText n) :: NodeName "td" el :: [] -> Ok(n, getText el)
                    | _ ->
                        nodes
                        |> List.map getOuterHtml
                        |> Logging.dumpt "title did not match"

                        Error "title not found"

                let (|Title|_|) (nodes: _ list) =
                    match getTitle nodes with
                    | Ok x -> Some x
                    | _ -> None

                try
                    match x with
                    | Title (cat, pwr) :: rem ->

                        let paired =
                            rem
                            |> List.map (function
                                | a :: b :: [] -> a, b
                                | _ -> failwith "pairing failed")

                        let talents =
                            paired |> List.map (fun (a, b) -> parseTalent a b)

                        let good =
                            talents
                            |> List.choose (function
                                | Ok x -> Some x
                                | _ -> None)

                        let bad =
                            talents
                            |> List.choose (function
                                | Error x -> Some x
                                | _ -> None)
                            |> List.map (fun (e1, e2) -> Error [ e1; e2 ])

                        Some(
                            { Name = cat
                              Power = pwr
                              Talents = good },
                            bad
                        )
                    | _ ->
                        dumpNodes "failing uhoh"
                        None

                with
                | ex ->
                    dumpNodes "failing "
                    ex |> Logging.dump
                    failwithf "Failed to map talents: %s" ex.Message)
        )
