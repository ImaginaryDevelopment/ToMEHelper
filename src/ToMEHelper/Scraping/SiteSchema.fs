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

let versionIdMap = Map [
    "tome-3.9.17", 5
    "tome-3.9.18", 41
    "tome-3.9.19", 44
    "tome-3.9.20", 52
    "tome-3.9.21", 53
    "bob-0.3.1", 54
    "tome-3.9.22", 55
    "tome-3.9.23", 57
    "tome-3.9.24", 58
    "tome-3.9.25", 59
    "tome-3.9.26", 60
    "tome-3.9.27", 61
    "tome-3.9.28", 62
    "tome-3.9.29", 63
    "tome-3.9.30", 64
    "tome-3.9.31", 67
    "tome-3.9.32", 69
    "tome-3.9.33", 73
    "tome-3.9.34", 75
    "tome-3.9.35", 77
    "tome-3.9.36", 85
    "tome-3.9.37", 88
    "tome-3.9.38", 90
    "tome-3.9.39", 93
    "tome-3.9.40", 94
    "tome-3.9.41", 96
    "tome-0.9.42", 101
    "tome-1.0.4", 105
    "tome-1.1.5", 109
    "tome-0.9.46", 113
    "tome-1.0.0", 115
    "tome-1.1.3", 116
    "tome-1.0.1", 119
    "tome-0.9.43", 125
    "tome-0.9.47", 133
    "tome-0.9.44", 136
    "tome-0.9.45", 141
    "tome-1.0.3", 142
    "tome-1.0.5", 146
    "tome-1.1.1", 149
    "tome-1.1.0", 150
    "tome-1.1.2", 153
    "tome-1.1.4", 156
    "tome-1.0.2", 177
    "tome-1.0.6", 181
    "tome-1.2.0", 12399
    "tome-1.2.2", 12401
    "tome-1.2.1", 12468
    "tome-1.2.5", 12498
    "tome-1.2.4", 12596
    "tome-1.4.0", 12614
    "tome-1.2.3", 12618
    "tome-1.3.0", 12682
    "tome-1.3.1", 12754
    "tome-1.4.2", 12765
    "tome-1.4.4", 12803
    "tome-1.5.1", 12844
    "tome-1.5.5", 12856
    "tome-1.4.9", 12871
    "tome-1.4.5", 13322
    "tome-1.5.10", 13400
    "tome-1.5.0", 13682
    "tome-1.4.8", 13700
    "tome-1.4.3", 13985
    "tome-1.4.6", 14287
    "tome-1.3.3", 14445
    "tome-1.5.3", 16906
    "tome-1.5.8", 17056
    "tome-1.5.2", 18784
    "tome-1.3.2", 22529
    "tome-1.4.1", 28038
    "tome-1.5.4", 28789
    "tome-1.5.9", 31178
    "tome-1.5.6", 40485
    "tome-1.5.7", 42183
    "tome-1.4.7", 51905
    "tome-1.6.0", 99782
    "tome-1.6.1", 326742
    "tome-1.6.2", 335883
    "tome-1.6.3", 336292
    "tome-1.6.4", 337515
    "tome-1.6.5", 354379
    "tome-1.6.6", 368459
    "tome-1.6.7", 397637
    "tome-1.7.0", 532301
    "tome-1.7.1", 545101
    "tome-1.7.2", 545529
    "tome-1.7.3", 673614
    "tome-1.7.4", 699172
    ]

let classFromId = Map [
    104, Adventurer
    19, Alchemist
    326744, Annihilator
    20, Anorithil
    22, ArcaneBlade
    14, Archer
    7, Archmage
    16, Berserker
    56, Brawler
    80, Bulwark
    34, Corruptor
    133921, CultistOfEntropy
    10, Cursed
    23297, Demonologist
    23313, Doombringer
    29, Doomed
    //  267, Dwarf
//  103389, Ghoul
    208, Gunslinger
    //  10201, Halfling
//  31036, Higher
//  815099, Insane
    71, Marauder
    48, Mindslayer
    68, Necromancer
    //  322125, Ogre
    179, Oozemancer
    //  94927, Orc
    43, ParadoxMage
    95691, Possessor
    67509, Psyshot
    31, Reaver
    12, Rogue
    67403, Sawbutcher
    23, Shadowblade
    //  284, Shalore
//  341234, Skeleton
    12400, Skirmisher
    102, Solipsist
    70, StoneWarden
    17, Summoner
    27, SunPaladin
    49, TemporalWarden
    104071, WrithingOne
    4, Wyrmic
]

let campaignFromId = Map [
    2, Maj
    46, Arena
    24, Infinite
    67402, Orcs
]

let difficultyFromId = Map [
    33, Easy
    6, Normal
    26, Nightmare
    227, Madness
    36, Insane
]

let permadeathFromId = Map [
    72, Exploration
    65, Adventure
    66, Roguelike
]

// http://zigur.te4.org/ claims the race mappings may be different
let raceFromId = Map [
    114, Orc
    133993, Krog
    25, Skeleton
    42, Ghoul
    104070, Drem
    74, Lich
    21, Halfling
    47, Yeek
    67497, Whitehoof
    18, Shalore
    3, Higher
    9, Dwarf
    8, Cornac
    23296, Doomelf
    37515, Ogre
    13, Thalore
    67413, KrukYeti
]
