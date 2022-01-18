module ToMEHelper.Scraping.SiteSchema

open ToMEHelper.Schema


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

// http://zigur.te4.org/ claims the race mappings may be different
let getRaceFromId =
    function
    | 114 -> Ok Orc
    | 133993 -> Ok Krog
    | 25 -> Ok Skeleton
    | 42 -> Ok Ghoul
    | 104070 -> Ok Drem
    | 74 -> Ok Lich
    | 21 -> Ok Halfling
    | 47 -> Ok Yeek
    | 67497 -> Ok Whitehoof
    | 18 -> Ok Shalore
    | 3 -> Ok Higher
    | 9 -> Ok Dwarf
    | 8 -> Ok Cornac
    | 23296 -> Ok Doomelf
    | 37515 -> Ok Ogre
    | 13 -> Ok Thalore
    | x -> Error x