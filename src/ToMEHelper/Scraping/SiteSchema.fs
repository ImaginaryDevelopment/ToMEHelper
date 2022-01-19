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

let getRaceId =
    function
    | Cornac -> 8
    | Doomelf -> 23296
    | Drem -> 104070
    | Dwarf -> 9
    | Ghoul -> 42
    | Halfling -> 21
    | Higher -> 3
    | Krog -> 133993
    | KrukYeti -> 67413
    | Lich -> 74
    | Ogre -> 37515
    | Orc -> 67402
    | Shalore -> 18
    | Skeleton -> 25
    | Thalore -> 13
    | Whitehoof -> 67497
    | Yeek -> 47

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

let getVersionId =
    function
    | "tome-3.9.17" -> Some 5
    | "tome-3.9.18" -> Some 41
    | "tome-3.9.19" -> Some 44
    | "tome-3.9.20" -> Some 52
    | "tome-3.9.21" -> Some 53
    | "bob-0.3.1" -> Some 54
    | "tome-3.9.22" -> Some 55
    | "tome-3.9.23" -> Some 57
    | "tome-3.9.24" -> Some 58
    | "tome-3.9.25" -> Some 59
    | "tome-3.9.26" -> Some 60
    | "tome-3.9.27" -> Some 61
    | "tome-3.9.28" -> Some 62
    | "tome-3.9.29" -> Some 63
    | "tome-3.9.30" -> Some 64
    | "tome-3.9.31" -> Some 67
    | "tome-3.9.32" -> Some 69
    | "tome-3.9.33" -> Some 73
    | "tome-3.9.34" -> Some 75
    | "tome-3.9.35" -> Some 77
    | "tome-3.9.36" -> Some 85
    | "tome-3.9.37" -> Some 88
    | "tome-3.9.38" -> Some 90
    | "tome-3.9.39" -> Some 93
    | "tome-3.9.40" -> Some 94
    | "tome-3.9.41" -> Some 96
    | "tome-0.9.42" -> Some 101
    | "tome-1.0.4" -> Some 105
    | "tome-1.1.5" -> Some 109
    | "tome-0.9.46" -> Some 113
    | "tome-1.0.0" -> Some 115
    | "tome-1.1.3" -> Some 116
    | "tome-1.0.1" -> Some 119
    | "tome-0.9.43" -> Some 125
    | "tome-0.9.47" -> Some 133
    | "tome-0.9.44" -> Some 136
    | "tome-0.9.45" -> Some 141
    | "tome-1.0.3" -> Some 142
    | "tome-1.0.5" -> Some 146
    | "tome-1.1.1" -> Some 149
    | "tome-1.1.0" -> Some 150
    | "tome-1.1.2" -> Some 153
    | "tome-1.1.4" -> Some 156
    | "tome-1.0.2" -> Some 177
    | "tome-1.0.6" -> Some 181
    | "tome-1.2.0" -> Some 12399
    | "tome-1.2.2" -> Some 12401
    | "tome-1.2.1" -> Some 12468
    | "tome-1.2.5" -> Some 12498
    | "tome-1.2.4" -> Some 12596
    | "tome-1.4.0" -> Some 12614
    | "tome-1.2.3" -> Some 12618
    | "tome-1.3.0" -> Some 12682
    | "tome-1.3.1" -> Some 12754
    | "tome-1.4.2" -> Some 12765
    | "tome-1.4.4" -> Some 12803
    | "tome-1.5.1" -> Some 12844
    | "tome-1.5.5" -> Some 12856
    | "tome-1.4.9" -> Some 12871
    | "tome-1.4.5" -> Some 13322
    | "tome-1.5.10" -> Some 13400
    | "tome-1.5.0" -> Some 13682
    | "tome-1.4.8" -> Some 13700
    | "tome-1.4.3" -> Some 13985
    | "tome-1.4.6" -> Some 14287
    | "tome-1.3.3" -> Some 14445
    | "tome-1.5.3" -> Some 16906
    | "tome-1.5.8" -> Some 17056
    | "tome-1.5.2" -> Some 18784
    | "tome-1.3.2" -> Some 22529
    | "tome-1.4.1" -> Some 28038
    | "tome-1.5.4" -> Some 28789
    | "tome-1.5.9" -> Some 31178
    | "tome-1.5.6" -> Some 40485
    | "tome-1.5.7" -> Some 42183
    | "tome-1.4.7" -> Some 51905
    | "tome-1.6.0" -> Some 99782
    | "tome-1.6.1" -> Some 326742
    | "tome-1.6.2" -> Some 335883
    | "tome-1.6.3" -> Some 336292
    | "tome-1.6.4" -> Some 337515
    | "tome-1.6.5" -> Some 354379
    | "tome-1.6.6" -> Some 368459
    | "tome-1.6.7" -> Some 397637
    | "tome-1.7.0" -> Some 532301
    | "tome-1.7.1" -> Some 545101
    | "tome-1.7.2" -> Some 545529
    | "tome-1.7.3" -> Some 673614
    | "tome-1.7.4" -> Some 699172
    | _ -> None

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
    | 67413 -> Ok KrukYeti
    | x -> Error x
