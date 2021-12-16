
module ToMEHelper.Scraping.CharactersTests

open System
open Expecto
open Expecto.ExpectoFsCheck
// open ToMEHelper
open FSharp.Reflection

open ToMEHelper.Schema
open ToMEHelper.Scraping.Characters
open ToMEHelper.Scraping.ParseHelpers

[<Tests>]
let loggingTests = testList "Characters Logging" [
    testCase "dump can be happy"
    <| fun _ ->
        Logging.dump "hello"
        |> ignore

    testCase "dumpt can be happy"
    <| fun _ ->
        Logging.dumpt "title" "hello"
]

open ToMEHelper.Scraping.Characters.Parsing

[<Tests>]
let charactersTests = testList "Characters" [
    testList "parseTalent" [
        let sampleData = """<td><ul><li class='qtip-link'><div class='qtip-tooltip'><img style='float:right' src='/images/gfx-tome//talents/warshout.png'><span style='color: #6FFF83;'>Effective talent level: </span><span style='color: #00FF00;'>9.8<br>
    </span><span>Use mode: </span><span style='color: #00FF00;'>Activated<br>
    </span><span>Stamina cost: </span><span style='color: #ffcc80;'>30<br>
    </span><span>Range: </span><span style='color: #FFFFFF;'>melee/personal<br>
    </span><span>Cooldown: </span><span style='color: #FFFFFF;'>16<br>
    </span><span>Travel Speed: </span><span style='color: #FFFFFF;'>instantaneous<br>
    </span><span>Usage Speed: </span><span style='color: #FFFFFF;'>Weapon (</span><span style='color: rgb(0,255,0);'>10%</span><span style='color: #LAST;'> of a turn)<br>
    </span><span>Description: </span><span style='color: #FFFFFF;'>Shout your warcry in a frontal cone of radius 10. Any targets caught inside will be confused (50% confusion power) for 10 turns.</span></div>Warshout</li></ul></td>"""
        let spentData = """<td>5/5</td>"""
        let div = sprintf "<html><body><div id=\"node\">%s%s</div></body></html>" sampleData spentData |> parseHtml |> getElementById "node"
        testCase "can be happy"
        <| fun _ ->
            let s1 = div.ChildNodes.[0]
            let s2 = div.ChildNodes.[1]
            Parsing.parseTalent s1 s2
            |> function
                | Error e -> failwithf "Failed to parse talent:%A" e
                | Ok x ->
                    Expect.equal x.Name "Warshout" null
                    Expect.equal x.Points 5 null
    ]
    testList "mapTalents" [
        let sample = """<div class="charsheet" style="">
<h4>Class Talents</h4>
<table class="talents">
<tr><td><strong>Technique / Berserker's strength</strong></td><td>1.50</td></tr><tr><td><ul><li class='qtip-link'><div class='qtip-tooltip'><img style='float:right' src='/images/gfx-tome//talents/warshout.png'><span style='color: #6FFF83;'>Effective talent level: </span><span style='color: #00FF00;'>9.8<br>
</span><span style='color: #6FFF83;'>Use mode: </span><span style='color: #00FF00;'>Activated<br>
</span><span style='color: #6FFF83;'>Stamina cost: </span><span style='color: #ffcc80;'>30<br>
</span><span style='color: #6FFF83;'>Range: </span><span style='color: #FFFFFF;'>melee/personal<br>
</span><span style='color: #6FFF83;'>Cooldown: </span><span style='color: #FFFFFF;'>16<br>
</span><span style='color: #6FFF83;'>Travel Speed: </span><span style='color: #FFFFFF;'>instantaneous<br>
</span><span style='color: #6FFF83;'>Usage Speed: </span><span style='color: #FFFFFF;'>Weapon (</span><span style='color: rgb(0,255,0);'>10%</span><span style='color: #LAST;'> of a turn)<br>
</span><span style='color: #6FFF83;'>Description: </span><span style='color: #FFFFFF;'>Shout your warcry in a frontal cone of radius 10. Any targets caught inside will be confused (50% confusion power) for 10 turns.</span></div>Warshout</li></ul></td><td>5/5</td></tr><tr><td><ul><li class='qtip-link'><div class='qtip-tooltip'><img style='float:right' src='/images/gfx-tome//talents/berserker.png'><span style='color: #6FFF83;'>Effective talent level: </span><span style='color: #00FF00;'>9.8<br>
</span><span style='color: #6FFF83;'>Use mode: </span><span style='color: #00FF00;'>Sustained<br>
</span><span style='color: #6FFF83;'>Sustain stamina cost: </span><span style='color: #ffcc80;'>20<br>
</span><span style='color: #6FFF83;'>Range: </span><span style='color: #FFFFFF;'>melee/personal<br>
</span><span style='color: #6FFF83;'>Cooldown: </span><span style='color: #FFFFFF;'>9<br>
</span><span style='color: #6FFF83;'>Travel Speed: </span><span style='color: #FFFFFF;'>instantaneous<br>
</span><span style='color: #6FFF83;'>Usage Speed: </span><span style='color: #FFFFFF;'>Instant (</span><span style='color: rgb(0,255,0);'>0%</span><span style='color: #LAST;'> of a turn)<br>
</span><span style='color: #6FFF83;'>Description: </span><span style='color: #FFFFFF;'>You enter an aggressive battle rage, increasing Accuracy by 46 and Physical Power by 65 and making you nearly unstoppable, granting 64% stun and pinning resistance.<br>
Sustaining this rage takes its toll on your body, decreasing your life by 2% each turn, but for every 1% of life missing you gain 0.5% critical hit chance.<br>
Even when sustained, this talent is only active when foes are in sight.<br>
The Accuracy bonus increases with your Dexterity, and the Physical Power bonus with your Strength.</span></div>Berserker Rage</li></ul></td><td>5/5</td></tr><tr><td><ul><li class='qtip-link'><div class='qtip-tooltip'><img style='float:right' src='/images/gfx-tome//talents/sunder_armour.png'><span style='color: #6FFF83;'>Effective talent level: </span><span style='color: #00FF00;'>9.8<br>
</span><span style='color: #6FFF83;'>Use mode: </span><span style='color: #00FF00;'>Activated<br>
</span><span style='color: #6FFF83;'>Stamina cost: </span><span style='color: #ffcc80;'>12<br>
</span><span style='color: #6FFF83;'>Range: </span><span style='color: #FFFFFF;'>melee/personal<br>
</span><span style='color: #6FFF83;'>Cooldown: </span><span style='color: #FFFFFF;'>6<br>
</span><span style='color: #6FFF83;'>Travel Speed: </span><span style='color: #FFFFFF;'>instantaneous<br>
</span><span style='color: #6FFF83;'>Usage Speed: </span><span style='color: #FFFFFF;'>Weapon (</span><span style='color: rgb(0,255,0);'>10%</span><span style='color: #LAST;'> of a turn)<br>
</span><span style='color: #6FFF83;'>Description: </span><span style='color: #FFFFFF;'>Hits the target with your weapon, doing 178% damage. If the attack hits, the target's armour and saves are reduced by 44 for 11 turns.<br>
Also if the target is protected by a temporary damage shield there is 93% chance to shatter it.<br>
Armor reduction chance increases with your Physical Power.</span></div>Shattering Blow</li></ul></td><td>5/5</td></tr><tr><td><ul><li class='qtip-link'><div class='qtip-tooltip'><img style='float:right' src='/images/gfx-tome//talents/relentless_fury.png'><span style='color: #6FFF83;'>Effective talent level: </span><span style='color: #00FF00;'>9.8<br>
</span><span style='color: #6FFF83;'>Use mode: </span><span style='color: #00FF00;'>Activated<br>
</span><span style='color: #6FFF83;'>Range: </span><span style='color: #FFFFFF;'>10<br>
</span><span style='color: #6FFF83;'>Cooldown: </span><span style='color: #FFFFFF;'>22<br>
</span><span style='color: #6FFF83;'>Travel Speed: </span><span style='color: #FFFFFF;'>instantaneous<br>
</span><span style='color: #6FFF83;'>Usage Speed: </span><span style='color: #FFFFFF;'>Weapon (</span><span style='color: rgb(0,255,0);'>10%</span><span style='color: #LAST;'> of a turn)<br>
</span><span style='color: #6FFF83;'>Description: </span><span style='color: #FFFFFF;'>Search your inner strength for a surge of power.<br>
For 9 turns you gain 23 stamina per turn and 41% movement and attack speed.<br>
Only usable at 30% or lower stamina.<br>
Stamina regeneration is based on your Constitution stat.</span></div>Relentless Fury</li></ul></td><td>5/5</td></tr><tr><td><strong>Technique / Bloodthirst</strong></td><td>1.50</td></tr><tr><td><ul><li class='qtip-link'><div class='qtip-tooltip'><img style='float:right' src='/images/gfx-tome//talents/mortal_terror.png'><span style='color: #6FFF83;'>Effective talent level: </span><span style='color: #00FF00;'>9.8<br>
</span><span style='color: #6FFF83;'>Use mode: </span><span style='color: #00FF00;'>Passive<br>
</span><span style='color: #6FFF83;'>Description: </span><span style='color: #FFFFFF;'>Your mighty blows inspire utter terror on your foes. Any melee strike you do that deals more than 16% of the target's total life puts them in a mortal terror, dazing them for 5 turns.<br>
Your critical strike chance also increase by 22%.<br>
The daze chance increase with your Physical Power.</span></div>Mortal Terror</li></ul></td><td>5/5</td></tr><tr><td><ul><li class='qtip-link'><div class='qtip-tooltip'><img style='float:right' src='/images/gfx-tome//talents/bloodbath.png'><span style='color: #6FFF83;'>Effective talent level: </span><span style='color: #00FF00;'>9.8<br>
</span><span style='color: #6FFF83;'>Use mode: </span><span style='color: #00FF00;'>Passive<br>
</span><span style='color: #6FFF83;'>Description: </span><span style='color: #FFFFFF;'>Delight in spilling the blood of your foes.  After scoring a critical hit, your maximum hit points will be increased by 17%, your life regeneration by 7.37 per turn, and your stamina regeneration by 1.47 per turn for 12 turns.<br>
The life and stamina regeneration will stack up to five times, for a maximum of 36.83 and 7.37 each turn, respectively.</span></div>Bloodbath</li></ul></td><td>5/5</td></tr><tr><td><ul><li class='qtip-link'><div class='qtip-tooltip'><img style='float:right' src='/images/gfx-tome//talents/bloody_butcher.png'><span style='color: #6FFF83;'>Effective talent level: </span><span style='color: #00FF00;'>9.8<br>
</span><span style='color: #6FFF83;'>Use mode: </span><span style='color: #00FF00;'>Passive<br>
</span><span style='color: #6FFF83;'>Description: </span><span style='color: #FFFFFF;'>You delight in the inflicting of wounds, providing 56 physical power.<br>
In addition when you make a creature bleed its physical damage resistance is reduced by 62% (but never below 0%).<br>
Physical power depends on your Strength stat.</span></div>Bloody Butcher</li></ul></td><td>5/5</td></tr><tr><td><ul><li class='qtip-link'><div class='qtip-tooltip'><img style='float:right' src='/images/gfx-tome//talents/unstoppable.png'><span style='color: #6FFF83;'>Effective talent level: </span><span style='color: #00FF00;'>9.8<br>
</span><span style='color: #6FFF83;'>Use mode: </span><span style='color: #00FF00;'>Activated<br>
</span><span style='color: #6FFF83;'>Stamina cost: </span><span style='color: #ffcc80;'>120<br>
</span><span style='color: #6FFF83;'>Range: </span><span style='color: #FFFFFF;'>melee/personal<br>
</span><span style='color: #6FFF83;'>Fixed Cooldown: </span><span style='color: #FFFFFF;'>45<br>
</span><span style='color: #6FFF83;'>Travel Speed: </span><span style='color: #FFFFFF;'>instantaneous<br>
</span><span style='color: #6FFF83;'>Usage Speed: </span><span style='color: #FFFFFF;'>Weapon (</span><span style='color: rgb(0,255,0);'>10%</span><span style='color: #LAST;'> of a turn)<br>
</span><span style='color: #6FFF83;'>Description: </span><span style='color: #FFFFFF;'>You enter a battle frenzy for 7 turns. During that time, you can not use items, healing has no effect, and your health cannot drop below 1.<br>
At the end of the frenzy, you regain 25% of your health per foe slain during the frenzy.<br>
While Unstoppable is active, Berserker Rage critical bonus is disabled as you lose the thrill of the risk of death.</span></div>Unstoppable</li></ul></td><td>5/5</td></tr><tr><td><strong>Technique / Superiority</strong></td><td>1.50</td></tr><tr><td><ul><li class='qtip-link'><div class='qtip-tooltip'><img style='float:right' src='/images/gfx-tome//talents/juggernaut.png'><span style='color: #6FFF83;'>Effective talent level: </span><span style='color: #00FF00;'>9.8<br>
</span><span style='color: #6FFF83;'>Use mode: </span><span style='color: #00FF00;'>Activated<br>
</span><span style='color: #6FFF83;'>Stamina cost: </span><span style='color: #ffcc80;'>50<br>
</span><span style='color: #6FFF83;'>Range: </span><span style='color: #FFFFFF;'>melee/personal<br>
</span><span style='color: #6FFF83;'>Cooldown: </span><span style='color: #FFFFFF;'>36<br>
</span><span style='color: #6FFF83;'>Travel Speed: </span><span style='color: #FFFFFF;'>instantaneous<br>
</span><span style='color: #6FFF83;'>Usage Speed: </span><span style='color: #FFFFFF;'>Instant (</span><span style='color: rgb(0,255,0);'>0%</span><span style='color: #LAST;'> of a turn)<br>
</span><span style='color: #6FFF83;'>Description: </span><span style='color: #FFFFFF;'>Concentrate on the battle, ignoring some of the damage you take.<br>
Improves physical damage reduction by 49% and provides a 31% chance to shrug off critical damage for 20 turns.</span></div>Juggernaut</li></ul></td><td>5/5</td></tr><tr><td><ul><li class='qtip-link'><div class='qtip-tooltip'><img style='float:right' src='/images/gfx-tome//talents/onslaught.png'><span style='color: #6FFF83;'>Effective talent level: </span><span style='color: #00FF00;'>9.8<br>
</span><span style='color: #6FFF83;'>Use mode: </span><span style='color: #00FF00;'>Sustained<br>
</span><span style='color: #6FFF83;'>Sustain stamina cost: </span><span style='color: #ffcc80;'>10<br>
</span><span style='color: #6FFF83;'>Range: </span><span style='color: #FFFFFF;'>6<br>
</span><span style='color: #6FFF83;'>Cooldown: </span><span style='color: #FFFFFF;'>9<br>
</span><span style='color: #6FFF83;'>Travel Speed: </span><span style='color: #FFFFFF;'>instantaneous<br>
</span><span style='color: #6FFF83;'>Usage Speed: </span><span style='color: #FFFFFF;'>Instant (</span><span style='color: rgb(0,255,0);'>0%</span><span style='color: #LAST;'> of a turn)<br>
</span><span style='color: #6FFF83;'>Description: </span><span style='color: #FFFFFF;'>Take an offensive stance. As you attack your foes, you knock your target and foes adjacent to them in a frontal arc back (up to 6 grids).<br>
This consumes stamina rapidly (-1 stamina/turn).</span></div>Onslaught</li></ul></td><td>5/5</td></tr><tr><td><ul><li class='qtip-link'><div class='qtip-tooltip'><img style='float:right' src='/images/gfx-tome//talents/battle_call.png'><span style='color: #6FFF83;'>Effective talent level: </span><span style='color: #00FF00;'>9.8<br>
</span><span style='color: #6FFF83;'>Use mode: </span><span style='color: #00FF00;'>Activated<br>
</span><span style='color: #6FFF83;'>Stamina cost: </span><span style='color: #ffcc80;'>30<br>
</span><span style='color: #6FFF83;'>Range: </span><span style='color: #FFFFFF;'>melee/personal<br>
</span><span style='color: #6FFF83;'>Cooldown: </span><span style='color: #FFFFFF;'>9<br>
</span><span style='color: #6FFF83;'>Travel Speed: </span><span style='color: #FFFFFF;'>instantaneous<br>
</span><span style='color: #6FFF83;'>Usage Speed: </span><span style='color: #FFFFFF;'>Weapon (</span><span style='color: rgb(0,255,0);'>10%</span><span style='color: #LAST;'> of a turn)<br>
</span><span style='color: #6FFF83;'>Description: </span><span style='color: #FFFFFF;'>Call all foes in a radius of 9 around you into battle, getting them into melee range in an instant.</span></div>Battle Call</li></ul></td><td>5/5</td></tr><tr><td><ul><li class='qtip-link'><div class='qtip-tooltip'><img style='float:right' src='/images/gfx-tome//talents/shattering_impact.png'><span style='color: #6FFF83;'>Effective talent level: </span><span style='color: #00FF00;'>9.8<br>
</span><span style='color: #6FFF83;'>Use mode: </span><span style='color: #00FF00;'>Sustained<br>
</span><span style='color: #6FFF83;'>Sustain stamina cost: </span><span style='color: #ffcc80;'>40<br>
</span><span style='color: #6FFF83;'>Range: </span><span style='color: #FFFFFF;'>melee/personal<br>
</span><span style='color: #6FFF83;'>Cooldown: </span><span style='color: #FFFFFF;'>27<br>
</span><span style='color: #6FFF83;'>Travel Speed: </span><span style='color: #FFFFFF;'>instantaneous<br>
</span><span style='color: #6FFF83;'>Usage Speed: </span><span style='color: #FFFFFF;'>Weapon (</span><span style='color: rgb(0,255,0);'>10%</span><span style='color: #LAST;'> of a turn)<br>
</span><span style='color: #6FFF83;'>Description: </span><span style='color: #FFFFFF;'>Put all of your strength into your weapon blows, creating shockwaves that deal 67% Physical weapon damage to all nearby targets.  Only one shockwave will be created per action, and the primary target does not take extra damage.<br>
Each shattering impact will drain 8 stamina.</span></div>Shattering Impact</li></ul></td><td>5/5</td></tr><tr><td><strong>Technique / Combat veteran</strong></td><td>1.50</td></tr><tr><td><ul><li class='qtip-link'><div class='qtip-tooltip'><img style='float:right' src='/images/gfx-tome//talents/quick_recovery.png'><span style='color: #6FFF83;'>Effective talent level: </span><span style='color: #00FF00;'>9.8<br>
</span><span style='color: #6FFF83;'>Use mode: </span><span style='color: #00FF00;'>Passive<br>
</span><span style='color: #6FFF83;'>Description: </span><span style='color: #FFFFFF;'>Your combat focus allows you to regenerate stamina faster (+4.3 stamina/turn).</span></div>Quick Recovery</li></ul></td><td>5/5</td></tr><tr><td><ul><li class='qtip-link'><div class='qtip-tooltip'><img style='float:right' src='/images/gfx-tome//talents/fast_metabolism.png'><span style='color: #6FFF83;'>Effective talent level: </span><span style='color: #00FF00;'>9.8<br>
</span><span style='color: #6FFF83;'>Use mode: </span><span style='color: #00FF00;'>Passive<br>
</span><span style='color: #6FFF83;'>Description: </span><span style='color: #FFFFFF;'>Your combat focus allows you to regenerate life faster (+11.7 life/turn).</span></div>Fast Metabolism</li></ul></td><td>5/5</td></tr><tr><td><ul><li class='qtip-link'><div class='qtip-tooltip'><img style='float:right' src='/images/gfx-tome//talents/spell_shield.png'><span style='color: #6FFF83;'>Effective talent level: </span><span style='color: #00FF00;'>9.8<br>
</span><span style='color: #6FFF83;'>Use mode: </span><span style='color: #00FF00;'>Passive<br>
</span><span style='color: #6FFF83;'>Description: </span><span style='color: #FFFFFF;'>Rigorous training allows you to be more resistant to some spell effects (+73 spell save).</span></div>Spell Shield</li></ul></td><td>5/5</td></tr><tr><td><ul><li class='qtip-link'><div class='qtip-tooltip'><img style='float:right' src='/images/gfx-tome//talents/unending_frenzy.png'><span style='color: #6FFF83;'>Effective talent level: </span><span style='color: #00FF00;'>9.8<br>
</span><span style='color: #6FFF83;'>Use mode: </span><span style='color: #00FF00;'>Passive<br>
</span><span style='color: #6FFF83;'>Description: </span><span style='color: #FFFFFF;'>You revel in the death of your foes, regaining 30.8 stamina with each death you cause.</span></div>Unending Frenzy</li></ul></td><td>5/5</td></tr><tr><td><strong>Technique / Warcries</strong></td><td>1.50</td></tr><tr><td><ul><li class='qtip-link'><div class='qtip-tooltip'><img style='float:right' src='/images/gfx-tome//talents/shattering_shout.png'><span style='color: #6FFF83;'>Effective talent level: </span><span style='color: #00FF00;'>9.8<br>
</span><span style='color: #6FFF83;'>Use mode: </span><span style='color: #00FF00;'>Activated<br>
</span><span style='color: #6FFF83;'>Stamina cost: </span><span style='color: #ffcc80;'>20<br>
</span><span style='color: #6FFF83;'>Range: </span><span style='color: #FFFFFF;'>melee/personal<br>
</span><span style='color: #6FFF83;'>Cooldown: </span><span style='color: #FFFFFF;'>6<br>
</span><span style='color: #6FFF83;'>Travel Speed: </span><span style='color: #FFFFFF;'>instantaneous<br>
</span><span style='color: #6FFF83;'>Usage Speed: </span><span style='color: #FFFFFF;'>Weapon (</span><span style='color: rgb(0,255,0);'>10%</span><span style='color: #LAST;'> of a turn)<br>
</span><span style='color: #6FFF83;'>Description: </span><span style='color: #FFFFFF;'>Release a powerful shout, doing 439.37 physical damage in a radius 10 cone in front of you.<br>
At level 5 the shout is so strong it shatters all incomming projectiles caught inside.<br>
The damage increases with your Strength.</span></div>Shattering Shout</li></ul></td><td>5/5</td></tr><tr><td><ul><li class='qtip-link'><div class='qtip-tooltip'><img style='float:right' src='/images/gfx-tome//talents/second_wind.png'><span style='color: #6FFF83;'>Effective talent level: </span><span style='color: #00FF00;'>9.8<br>
</span><span style='color: #6FFF83;'>Use mode: </span><span style='color: #00FF00;'>Activated<br>
</span><span style='color: #6FFF83;'>Range: </span><span style='color: #FFFFFF;'>melee/personal<br>
</span><span style='color: #6FFF83;'>Cooldown: </span><span style='color: #FFFFFF;'>45<br>
</span><span style='color: #6FFF83;'>Travel Speed: </span><span style='color: #FFFFFF;'>instantaneous<br>
</span><span style='color: #6FFF83;'>Usage Speed: </span><span style='color: #FFFFFF;'>Instant (</span><span style='color: rgb(0,255,0);'>0%</span><span style='color: #LAST;'> of a turn)<br>
</span><span style='color: #6FFF83;'>Description: </span><span style='color: #FFFFFF;'>Take a deep breath to recover 368 stamina.  The stamina recovery improves with your Strength and Willpower.</span></div>Second Wind</li></ul></td><td>5/5</td></tr><tr><td><ul><li class='qtip-link'><div class='qtip-tooltip'><img style='float:right' src='/images/gfx-tome//talents/battle_shout.png'><span style='color: #6FFF83;'>Effective talent level: </span><span style='color: #00FF00;'>9.8<br>
</span><span style='color: #6FFF83;'>Use mode: </span><span style='color: #00FF00;'>Activated<br>
</span><span style='color: #6FFF83;'>Stamina cost: </span><span style='color: #ffcc80;'>5<br>
</span><span style='color: #6FFF83;'>Range: </span><span style='color: #FFFFFF;'>melee/personal<br>
</span><span style='color: #6FFF83;'>Cooldown: </span><span style='color: #FFFFFF;'>27<br>
</span><span style='color: #6FFF83;'>Travel Speed: </span><span style='color: #FFFFFF;'>instantaneous<br>
</span><span style='color: #6FFF83;'>Usage Speed: </span><span style='color: #FFFFFF;'>Weapon (</span><span style='color: rgb(0,255,0);'>10%</span><span style='color: #LAST;'> of a turn)<br>
</span><span style='color: #6FFF83;'>Description: </span><span style='color: #FFFFFF;'>Boost your life and stamina by 29.1% for 17 turns by bellowing your battle shout.<br>
When the effect ends, the additional life and stamina will be lost.</span></div>Battle Shout</li></ul></td><td>5/5</td></tr><tr><td><ul><li class='qtip-link'><div class='qtip-tooltip'><img style='float:right' src='/images/gfx-tome//talents/battle_cry.png'><span style='color: #6FFF83;'>Effective talent level: </span><span style='color: #00FF00;'>9.8<br>
</span><span style='color: #6FFF83;'>Use mode: </span><span style='color: #00FF00;'>Activated<br>
</span><span style='color: #6FFF83;'>Stamina cost: </span><span style='color: #ffcc80;'>40<br>
</span><span style='color: #6FFF83;'>Range: </span><span style='color: #FFFFFF;'>melee/personal<br>
</span><span style='color: #6FFF83;'>Cooldown: </span><span style='color: #FFFFFF;'>27<br>
</span><span style='color: #6FFF83;'>Travel Speed: </span><span style='color: #FFFFFF;'>instantaneous<br>
</span><span style='color: #6FFF83;'>Usage Speed: </span><span style='color: #FFFFFF;'>Weapon (</span><span style='color: rgb(0,255,0);'>10%</span><span style='color: #LAST;'> of a turn)<br>
</span><span style='color: #6FFF83;'>Description: </span><span style='color: #FFFFFF;'>Your battle cry shatters the will of your foes within a radius of 10, lowering their Defense by 68 for 7 turns, making them easier to hit.<br>
All evasion and concealment bonuses are also disabled.<br>
The chance to hit increases with your Physical Power.</span></div>Battle Cry</li></ul></td><td>5/5</td></tr><tr><td><strong>Technique / Combat techniques</strong></td><td>1.50</td></tr><tr><td><ul><li class='qtip-link'><div class='qtip-tooltip'><img style='float:right' src='/images/gfx-tome//talents/rush.png'><span style='color: #6FFF83;'>Effective talent level: </span><span style='color: #00FF00;'>9.8<br>
</span><span style='color: #6FFF83;'>Use mode: </span><span style='color: #00FF00;'>Activated<br>
</span><span style='color: #6FFF83;'>Stamina cost: </span><span style='color: #ffcc80;'>2<br>
</span><span style='color: #6FFF83;'>Range: </span><span style='color: #FFFFFF;'>12<br>
</span><span style='color: #6FFF83;'>Cooldown: </span><span style='color: #FFFFFF;'>9<br>
</span><span style='color: #6FFF83;'>Travel Speed: </span><span style='color: #FFFFFF;'>instantaneous<br>
</span><span style='color: #6FFF83;'>Usage Speed: </span><span style='color: #FFFFFF;'>Weapon (</span><span style='color: rgb(0,255,0);'>10%</span><span style='color: #LAST;'> of a turn)<br>
</span><span style='color: #6FFF83;'>Description: </span><span style='color: #FFFFFF;'>Rush toward a target enemy with incredible speed and perform a melee attack for 120% weapon damage that can daze the target for 3 turns if it hits.<br>
You must rush from at least 2 tiles away.</span></div>Rush</li></ul></td><td>5/5</td></tr><tr><td><ul><li class='qtip-link'><div class='qtip-tooltip'><img style='float:right' src='/images/gfx-tome//talents/precise_strikes.png'><span style='color: #6FFF83;'>Effective talent level: </span><span style='color: #00FF00;'>9.8<br>
</span><span style='color: #6FFF83;'>Use mode: </span><span style='color: #00FF00;'>Sustained<br>
</span><span style='color: #6FFF83;'>Sustain stamina cost: </span><span style='color: #ffcc80;'>30<br>
</span><span style='color: #6FFF83;'>Range: </span><span style='color: #FFFFFF;'>melee/personal<br>
</span><span style='color: #6FFF83;'>Cooldown: </span><span style='color: #FFFFFF;'>27<br>
</span><span style='color: #6FFF83;'>Travel Speed: </span><span style='color: #FFFFFF;'>instantaneous<br>
</span><span style='color: #6FFF83;'>Usage Speed: </span><span style='color: #FFFFFF;'>Weapon (</span><span style='color: rgb(0,255,0);'>10%</span><span style='color: #LAST;'> of a turn)<br>
</span><span style='color: #6FFF83;'>Description: </span><span style='color: #FFFFFF;'>You focus your strikes, reducing your attack speed by 10% and increasing your Accuracy by 43 and critical chance by 27%.<br>
The effects will increase with your Dexterity.</span></div>Precise Strikes</li></ul></td><td>5/5</td></tr><tr><td><ul><li class='qtip-link'><div class='qtip-tooltip'><img style='float:right' src='/images/gfx-tome//talents/perfect_strike.png'><span style='color: #6FFF83;'>Effective talent level: </span><span style='color: #00FF00;'>9.8<br>
</span><span style='color: #6FFF83;'>Use mode: </span><span style='color: #00FF00;'>Activated<br>
</span><span style='color: #6FFF83;'>Stamina cost: </span><span style='color: #ffcc80;'>25<br>
</span><span style='color: #6FFF83;'>Range: </span><span style='color: #FFFFFF;'>melee/personal<br>
</span><span style='color: #6FFF83;'>Cooldown: </span><span style='color: #FFFFFF;'>22<br>
</span><span style='color: #6FFF83;'>Travel Speed: </span><span style='color: #FFFFFF;'>instantaneous<br>
</span><span style='color: #6FFF83;'>Usage Speed: </span><span style='color: #FFFFFF;'>Instant (</span><span style='color: rgb(0,255,0);'>0%</span><span style='color: #LAST;'> of a turn)<br>
</span><span style='color: #6FFF83;'>Description: </span><span style='color: #FFFFFF;'>You have learned to focus your blows to hit your target, granting +156 accuracy and allowing you to attack creatures you cannot see without penalty for the next 7 turns.</span></div>Perfect Strike</li></ul></td><td>5/5</td></tr><tr><td><ul><li class='qtip-link'><div class='qtip-tooltip'><img style='float:right' src='/images/gfx-tome//talents/blinding_speed.png'><span style='color: #6FFF83;'>Effective talent level: </span><span style='color: #00FF00;'>9.8<br>
</span><span style='color: #6FFF83;'>Use mode: </span><span style='color: #00FF00;'>Activated<br>
</span><span style='color: #6FFF83;'>Stamina cost: </span><span style='color: #ffcc80;'>25<br>
</span><span style='color: #6FFF83;'>Range: </span><span style='color: #FFFFFF;'>melee/personal<br>
</span><span style='color: #6FFF83;'>Cooldown: </span><span style='color: #FFFFFF;'>49<br>
</span><span style='color: #6FFF83;'>Travel Speed: </span><span style='color: #FFFFFF;'>instantaneous<br>
</span><span style='color: #6FFF83;'>Usage Speed: </span><span style='color: #FFFFFF;'>Instant (</span><span style='color: rgb(0,255,0);'>0%</span><span style='color: #LAST;'> of a turn)<br>
</span><span style='color: #6FFF83;'>Description: </span><span style='color: #FFFFFF;'>Through rigorous training, you have learned to focus your actions for a short while, increasing your speed by 59% for 5 turns.</span></div>Blinding Speed</li></ul></td><td>5/5</td></tr><tr><td><strong>Cunning / Dirty fighting</strong></td><td>1.20</td></tr><tr><td><ul><li class='qtip-link'><div class='qtip-tooltip'><img style='float:right' src='/images/gfx-tome//talents/dirty_fighting.png'><span style='color: #6FFF83;'>Effective talent level: </span><span style='color: #00FF00;'>7.8<br>
</span><span style='color: #6FFF83;'>Use mode: </span><span style='color: #00FF00;'>Activated<br>
</span><span style='color: #6FFF83;'>Stamina cost: </span><span style='color: #ffcc80;'>12<br>
</span><span style='color: #6FFF83;'>Range: </span><span style='color: #FFFFFF;'>melee/personal<br>
</span><span style='color: #6FFF83;'>Cooldown: </span><span style='color: #FFFFFF;'>9<br>
</span><span style='color: #6FFF83;'>Travel Speed: </span><span style='color: #FFFFFF;'>instantaneous<br>
</span><span style='color: #6FFF83;'>Usage Speed: </span><span style='color: #FFFFFF;'>Weapon (</span><span style='color: rgb(0,255,0);'>10%</span><span style='color: #LAST;'> of a turn)<br>
</span><span style='color: #6FFF83;'>Description: </span><span style='color: #FFFFFF;'>You make a low blow against a sensitive point on the target, dealing 225% unarmed damage. If your attack hits, the target is left reeling and vulnerable, reducing their physical save by 34 and their stun, blind, confusion and pin immunities to 50% of normal for 4 turns.<br>
This effect bypasses saves.</span></div>Dirty Fighting</li></ul></td><td>5/5</td></tr><tr><td><ul><li class='qtip-link'><div class='qtip-tooltip'><img style='float:right' src='/images/gfx-tome//talents/backstab.png'><span style='color: #6FFF83;'>Effective talent level: </span><span style='color: #00FF00;'>7.8<br>
</span><span style='color: #6FFF83;'>Use mode: </span><span style='color: #00FF00;'>Passive<br>
</span><span style='color: #6FFF83;'>Description: </span><span style='color: #FFFFFF;'>Your quick wit gives you a big advantage against disabled targets, increasing your damage by 13% for each disabling effect the target is under, to a maximum of 38%.<br>
For this purpose, disabling effects are stun, blind, daze, confuse, pin, disarm, cripple and silence.<br>
In addition, for each disabling effect the target is under, your melee attacks have a 8% (to a maximum of 25%) chance to inflict a new effect on them (that they do not already have): either disarm, cripple (25% power) or pin for 2 turns.<br>
The chance to further disable the target increases with your Accuracy.</span></div>Backstab</li></ul></td><td>5/5</td></tr><tr><td><ul><li class='qtip-link'><div class='qtip-tooltip'><img style='float:right' src='/images/gfx-tome//talents/blinding_powder.png'><span style='color: #6FFF83;'>Effective talent level: </span><span style='color: #00FF00;'>7.8<br>
</span><span style='color: #6FFF83;'>Use mode: </span><span style='color: #00FF00;'>Activated<br>
</span><span style='color: #6FFF83;'>Stamina cost: </span><span style='color: #ffcc80;'>18<br>
</span><span style='color: #6FFF83;'>Range: </span><span style='color: #FFFFFF;'>melee/personal<br>
</span><span style='color: #6FFF83;'>Cooldown: </span><span style='color: #FFFFFF;'>10<br>
</span><span style='color: #6FFF83;'>Travel Speed: </span><span style='color: #FFFFFF;'>instantaneous<br>
</span><span style='color: #6FFF83;'>Usage Speed: </span><span style='color: #FFFFFF;'>Standard (</span><span style='color: rgb(0,255,0);'>100%</span><span style='color: #LAST;'> of a turn)<br>
</span><span style='color: #6FFF83;'>Description: </span><span style='color: #FFFFFF;'>Throw a cloud of blinding dust in a radius 3 cone. Enemies within will be blinded, as well as having their accuracy reduced by 34 and movement speed decreased by 73% for 5 turns.<br>
The chance to inflict these effects increase with your Accuracy.</span></div>Blinding Powder</li></ul></td><td>5/5</td></tr><tr><td><ul><li class='qtip-link'><div class='qtip-tooltip'><img style='float:right' src='/images/gfx-tome//talents/twist_the_knife.png'><span style='color: #6FFF83;'>Effective talent level: </span><span style='color: #00FF00;'>7.8<br>
</span><span style='color: #6FFF83;'>Use mode: </span><span style='color: #00FF00;'>Activated<br>
</span><span style='color: #6FFF83;'>Stamina cost: </span><span style='color: #ffcc80;'>20<br>
</span><span style='color: #6FFF83;'>Range: </span><span style='color: #FFFFFF;'>melee/personal<br>
</span><span style='color: #6FFF83;'>Fixed Cooldown: </span><span style='color: #FFFFFF;'>15<br>
</span><span style='color: #6FFF83;'>Travel Speed: </span><span style='color: #FFFFFF;'>instantaneous<br>
</span><span style='color: #6FFF83;'>Usage Speed: </span><span style='color: #FFFFFF;'>Weapon (</span><span style='color: rgb(0,255,0);'>10%</span><span style='color: #LAST;'> of a turn)<br>
</span><span style='color: #6FFF83;'>Description: </span><span style='color: #FFFFFF;'>Make a painful strike dealing 162% weapon damage that increases the duration of up to 3 negative effect(s) on the target by 6 turns. For each negative effect extended this way, the duration of a beneficial effect is reduced by the same amount, possibly canceling it.</span></div>Twist the Knife</li></ul></td><td>5/5</td></tr><tr><td><strong>Technique / Two-handed assault</strong></td><td>1.50</td></tr><tr><td><ul><li class='qtip-link'><div class='qtip-tooltip'><img style='float:right' src='/images/gfx-tome//talents/stunning_blow.png'><span style='color: #6FFF83;'>Effective talent level: </span><span style='color: #00FF00;'>9.8<br>
</span><span style='color: #6FFF83;'>Use mode: </span><span style='color: #00FF00;'>Activated<br>
</span><span style='color: #6FFF83;'>Stamina cost: </span><span style='color: #ffcc80;'>8<br>
</span><span style='color: #6FFF83;'>Range: </span><span style='color: #FFFFFF;'>melee/personal<br>
</span><span style='color: #6FFF83;'>Cooldown: </span><span style='color: #FFFFFF;'>7<br>
</span><span style='color: #6FFF83;'>Travel Speed: </span><span style='color: #FFFFFF;'>instantaneous<br>
</span><span style='color: #6FFF83;'>Usage Speed: </span><span style='color: #FFFFFF;'>Weapon (</span><span style='color: rgb(0,255,0);'>10%</span><span style='color: #LAST;'> of a turn)<br>
</span><span style='color: #6FFF83;'>Description: </span><span style='color: #FFFFFF;'>Hit the target twice with your two-handed weapon, doing 78% damage. Each hit will try to stun the target for 9 turns.<br>
The stun chance increases with your Physical Power.</span></div>Stunning Blow</li></ul></td><td>5/5</td></tr><tr><td><ul><li class='qtip-link'><div class='qtip-tooltip'><img style='float:right' src='/images/gfx-tome//talents/fearless_cleave.png'><span style='color: #6FFF83;'>Effective talent level: </span><span style='color: #00FF00;'>9.8<br>
</span><span style='color: #6FFF83;'>Use mode: </span><span style='color: #00FF00;'>Activated<br>
</span><span style='color: #6FFF83;'>Stamina cost: </span><span style='color: #ffcc80;'>20<br>
</span><span style='color: #6FFF83;'>Range: </span><span style='color: #FFFFFF;'>melee/personal<br>
</span><span style='color: #6FFF83;'>Cooldown: </span><span style='color: #FFFFFF;'>0<br>
</span><span style='color: #6FFF83;'>Travel Speed: </span><span style='color: #FFFFFF;'>instantaneous<br>
</span><span style='color: #6FFF83;'>Usage Speed: </span><span style='color: #FFFFFF;'>Weapon (</span><span style='color: rgb(0,255,0);'>10%</span><span style='color: #LAST;'> of a turn)<br>
</span><span style='color: #6FFF83;'>Description: </span><span style='color: #FFFFFF;'>Take a step toward your foes then use the momentum to cleave all creatures adjacent to you for 156% weapon damage.</span></div>Fearless Cleave</li></ul></td><td>5/5</td></tr><tr><td><ul><li class='qtip-link'><div class='qtip-tooltip'><img style='float:right' src='/images/gfx-tome//talents/death_dance.png'><span style='color: #6FFF83;'>Effective talent level: </span><span style='color: #00FF00;'>9.8<br>
</span><span style='color: #6FFF83;'>Use mode: </span><span style='color: #00FF00;'>Activated<br>
</span><span style='color: #6FFF83;'>Stamina cost: </span><span style='color: #ffcc80;'>30<br>
</span><span style='color: #6FFF83;'>Range: </span><span style='color: #FFFFFF;'>melee/personal<br>
</span><span style='color: #6FFF83;'>Cooldown: </span><span style='color: #FFFFFF;'>9<br>
</span><span style='color: #6FFF83;'>Travel Speed: </span><span style='color: #FFFFFF;'>instantaneous<br>
</span><span style='color: #6FFF83;'>Usage Speed: </span><span style='color: #FFFFFF;'>Weapon (</span><span style='color: rgb(0,255,0);'>10%</span><span style='color: #LAST;'> of a turn)<br>
</span><span style='color: #6FFF83;'>Description: </span><span style='color: #FFFFFF;'>Spin around, extending your weapon in radius 2 and damaging all targets around you for 238% weapon damage.<br>
At level 3 all damage done will also make the targets bleed for an additional 150% damage over 5 turns</span></div>Death Dance</li></ul></td><td>5/5</td></tr><tr><td><ul><li class='qtip-link'><div class='qtip-tooltip'><img style='float:right' src='/images/gfx-tome//talents/execution.png'><span style='color: #6FFF83;'>Effective talent level: </span><span style='color: #00FF00;'>9.8<br>
</span><span style='color: #6FFF83;'>Use mode: </span><span style='color: #00FF00;'>Activated<br>
</span><span style='color: #6FFF83;'>Stamina cost: </span><span style='color: #ffcc80;'>25<br>
</span><span style='color: #6FFF83;'>Range: </span><span style='color: #FFFFFF;'>melee/personal<br>
</span><span style='color: #6FFF83;'>Cooldown: </span><span style='color: #FFFFFF;'>7<br>
</span><span style='color: #6FFF83;'>Travel Speed: </span><span style='color: #FFFFFF;'>instantaneous<br>
</span><span style='color: #6FFF83;'>Usage Speed: </span><span style='color: #FFFFFF;'>Weapon (</span><span style='color: rgb(0,255,0);'>10%</span><span style='color: #LAST;'> of a turn)<br>
</span><span style='color: #6FFF83;'>Description: </span><span style='color: #FFFFFF;'>Takes advantage of a wounded foe to perform a killing strike.  This attack is an automatic critical hit that does 3.1% extra weapon damage for each % of life the target is below maximum.<br>
(A victim with 30% remaining life (70% damaged) would take 318.6% weapon damage.)<br>
If an enemy dies from this attack then two of your talent cooldowns are reduced by 2 turns and Execution's cooldown is reset.</span></div>Execution</li></ul></td><td>5/5</td></tr></table>
</div>"""
        testCase "can be happy"
        <| fun _ ->
            TestHelpers.htmlNodesOfText sample
            |> List.head
            |> mapTalents
            |> List.ofSeq
            |> function
                | (tc,_)::_ ->
                    Expect.equal tc.Name "Technique / Berserker's strength" null
                    Expect.equal tc.Power "1.50" null
                | _ -> failwith "Unable to map talents"
            ()
    ]
]