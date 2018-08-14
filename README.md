# pokemon battle simulator

This is my own pokemon battle simulator.  Written in Haskell which I
wish I'd learned a long time ago.

This README is just a brief overview.  But I'm trying to make it more
comprehensive because I'll probably need it.

The calculations and defender's move selection are based on (IIRC)
Silph Road battle analysis posts.  Dodging is not supported.  I never
dodge because a) I don't see well enough and b) dodging lowers DPS.

I'm not sure how exceedingly accurate it is but I ve been using it to
help choose raid counters for a long time now and I'm never
disappointed.  Since AFAIK the defender chooses its charge move
somewhat randomly its impossible to be accurate anyway.

Damagage can be sorted by DPS (best for raids), TDO (if you're
wonderintg what to use against Blissey), and a couple other options.
It can show breakpoints, alternative movesets, powerups, and rank
movesets for a given matchup.

It handles weather and raid bosses.

My own pokemon are kept in a yaml file (for easy editing) that is
loaded on startup.  Each pokemon has name, species, level, IVs, and
moveset.  Level and IVs can be calculated from CP, HP, and appraisal.
If there are multiple possobilities for level and/or IVs, all
possibilities are simulated and worst-case DPS/TDO values are used.

This program is surprisingly fast.  It can crank through thousands of
simulated battles per second, which is great for simulating
alternative movesets and powerups.

There are a fair amount of other programs here too, for things like
updating the yaml file for new pokemon or after powerups.  There are
also some programs that plot DPS and TDO for varying IVs.  The main
takeaway from the plots is by far the most important IV, and that a
better defense IV and/or powering up my actualy make a pookemon do
worse depending on the matchup.

# The my_pokemon.yaml file

Pokemon are stored in a file in yaml format which may be created and
modified with any text editor.  By default the file is called `my_pokemon.yaml`
in the cirrent directory.

## Specifiying the my_pokemon.yaml file

The `my_pokemon.yaml` filename may be specified explicitly with the
`-f <filename>` option.  This is especialy useful if you multiple
accounts, each with its own file.  The `-f` option may also be given
multiple times and all files will be read as if they were
concatenated.

## Contents of my_pokremon.yaml

`my_pokemon.yaml` has one entry for each of each pokemon that you want
to use for battle simulations.  An entry looks likes this:

```
- name: Mutie
  species: mewtwo
  cp: 3525
  hp: 155
  dust: 7000
  quick: confusion
  charge: focus blast
  appraisal: wonder, hp, incredible
  ivs:
  - level: 33
    attack: 13
    defense: 11
    stamina: 15
```

The name is completely arbitrary but will generally be the name you've
given the pokemon, or the default name which is just the species.

For simulations, only the `species` and `ivs` are actually used.  Note
that `ivs` also includes the level.

If the `ivs` can't be determined unambiguously, then mu;ltiple `ivs`
blocks may be given.  All sets of `ivs` will be simulated and the
worst-case results will be used.

## Calculating the `ivs` wih the `ivs` utility

You can use an app like Poke Genie to calculate `ivs`.  Or you can do
what I do and use the `ivs` utility.  Put the `name  `species`, `cp`,
`hp`, and `appraisal` values in the file then run `ivs` like this:

```
ivs [-n] <your_file.yaml> > <new_file.yaml>
```

Use the `-n` switch if the pokemon has never been powered up.  This
will restrict the computed level(s) to whole numbers and exclude
half-levels.

The possible `ivs` will be computed for all pokemon in the file and
the results written to the standard output.

If a pokemon's `ivs` were unknown then an `ivs` block will be created
for each possible combination.

If a pokemon's alrezdy had `ivs` then they are retained except that
combinations that aren't possible will be removed.  This is useful when
you power up a pokemon and update its `cp` and `hp`.  Running `ivs` will
remove any ambiguous `ivs` that are no longer possible given the new
`cp` and `hp`.

### Specifying the `appraisal`

The `appraisal` looks like this:

```
wonder, hp, attack, incredible
```

It consists of the team leader's overall evaluation, the list of best
ivs (one, two, or three of `hp`, `attack`, and `defense`), and the
evaluation of the best ivs.  The evaluations depend on your team:

#### Mystic:

|Blanche's phrase includes|appraisal term|
|-------------------------|--------------|
|Your pokemon is a wonder!|wonder|
|has certainly caught my attention|attention|
|your pokemon is above average|above|
|not likely to make much headway in battle|not likely|

|Blanche's phrase includes|appraisal term|
|-------------------------|--------------|
|It's incredible!|incredible|
|I am certainly impressed|impressed|
|noticeably trending to the positive|trending|
|not out of the norm|not out|

#### Valor:

|Candela's phrase includes|appraisal term|
|-------------------------|--------------|
|amazes me|amazes|
|is a strong pokemon|strong|
|is a decent pokemon|decent|
|may not be great in battle|may not|

|Candela's phrase includes|appraisal term|
|-------------------------|--------------|
|I'm blown away|blown|
|It's got excellent stats|excellent|
|Its stats indicate|indicate|
|It's stats don't point to greatness|don't point|

If you're Instinct, you rock~ But you'll have to either use the Mystic
or Valor terms or edit src/Appraisal.hs (and submit a pull request).

## Examples

### What are the best counters to blizzard Kyogre tier 5 raid boss?

`-a`: simulate all pokemon species instead of `-f` to read pokemon
from a file.

`-g`: sort by DPS (glass cannon option)

`-M`: show moveset

Pokemon are simulated at level 30 by default.

This is for default (extreme) weather with no weather-boosted moves.

```
$ counter -g -M -a kyogre:r5/b
14.3   242  zapdos:30       charge beam  / thunderbolt    
14.0   237  raikou:30       thunder shock/ thunderbolt    
13.7   231  jolteon:30      thunder shock/ thunderbolt    
13.4   226  sceptile:30     fury cutter  / leaf blade     
12.8   216  rayquaza:30     dragon tail  / outrage        
12.8   216  victreebel:30   razor leaf   / leaf blade     
12.7   214  venusaur:30     vine whip    / frenzy plant   
12.6   213  shiftry:30      razor leaf   / leaf blade     
12.5   212  venusaur:30     razor leaf   / frenzy plant   
12.2   207  electabuzz:30   thunder shock/ thunderbolt    
12.2   206  breloom:30      bullet seed  / seed bomb      
```

### Where are their breakpoints?

Just add `-B`:

```
$ ./counter -B -g -M -a kyogre:r5/b
14.3   242  zapdos:30       charge beam  / thunderbolt    
14.0   237  raikou:30       thunder shock/ thunderbolt    
13.7   231  jolteon:30      thunder shock/ thunderbolt    
13.4   226  sceptile:30     fury cutter  / leaf blade     
12.8   216  rayquaza:30     dragon tail  / outrage        
12.8   216  victreebel:30   razor leaf   / leaf blade     
12.7   214  venusaur:30     vine whip    / frenzy plant   
12.6   213  shiftry:30      razor leaf   / leaf blade     
12.5   212  venusaur:30     razor leaf   / frenzy plant   
  30   8
  31   9
12.2   207  electabuzz:30   thunder shock/ thunderbolt    
```

So RL/FP Venusaur has a breakpoint at level 31, but the rest have reached
their final breakpoiint by level 30.  Let's see where those breakpoints are
alsoby using `-l 20`:

```
$ counter -l 20 -B -g -M -a kyogre:r5/b
12.2   207  raikou:20       thunder shock/ thunderbolt    
11.9   201  zapdos:20       charge beam  / thunderbolt    
  20   6
  28.5 7
11.2   222  venusaur:20     vine whip    / frenzy plant   
  20   4
  26.5 5
10.5   177  venusaur:20     razor leaf   / frenzy plant   
  20   7
  23.5 8
  31   9
10.5   176  sceptile:20     fury cutter  / leaf blade     
  20   1
  20.5 2
10.3   164  victreebel:20   razor leaf   / solar beam     
  20   7
  21.5 8
  28   9
10.3   170  raikou:20       thunder shock/ wild charge    
10.3   174  victreebel:20   razor leaf   / leaf blade     
  20   7
  21.5 8
  28   9
10.1   171  shiftry:20      razor leaf   / leaf blade     
  20   7
  23   8
  30   9
10.0   118  manectric:20    charge beam  / wild charge    
  20   5
  27   6
 9.9   130  jolteon:20      thunder shock/ thunderbolt    
  20   3
  21.5 4
```

# Plotting

There are several useful plots that can be done to help evaluate which
pokemon to evolve and possibly subsequently power up.  It often takes
125 candy to evolve a pokemon.  Would that candy (along with some
stardust) be better spent in powering up an existing pokemon, perhaps
one with lower IVs, instead?  And how far should you power up?  As
always it depends on the matchups, so these plots compare DPS or TDO
vs. stardust or candy costs by simulating particular matchups that you
choose.  In the end it's really subjective and depends on you candy
and stardust budget but the plots can help weed out the bad pokemon
and perhaps also spot some surprising stars.

## Here's how it works:

Create a file with the type of pokemon being considered, say
shiftry.yaml.  Make an entry for each shiftry, nuzleaf, and seedot
you're considering.  For the moveset, use the moveset you plan to have
on the fully evolved pokemon.  If the pokemon is already fully
evolved, use its existing moveset and also consider making another
entry with a different name and the ideal moveset.  Using square
brackets somewhere in the name will cause that pokeon to be plotted
with a dashed line, which is useful when using hypothetical pokemon
with an alternate moveet -- pokemon with solid lines repreent the
moveset you have, and pokemon with dashed lines are what you could get
with a TM.  Just remember not to get too excited if you see a dotted line
getting high samage -- you're going to have to TM it first.

Then do

```
plotpowerups.sh -p -s -t -f shiftry.yaml raichu_alola:r3
```

Ignore any libGL error messages; I have no idea what that's all about.

The command arguments are:

```-p``` run gnuplot to actually create a plot

```-c``` plot candy cost on the X axis

```-t``` plot TDO on the Y axis

```-f <filename>``` yaml file with the pokemon to plot

The final argument is the defending pokemon.  It can be of the usual form
```species[:level[/attack/defense/stamina]][:fast_move][/charge_move]```

Any unevolved pokemon are simulated as fully evolved.  For any pokemon
that needs to be evolved, the lowest candy cost will be the candy
needed to evolve, and will increase from there with power ups.  Fully
evolved pokemon will start from a candy cost of zero.

If a pokemon has multiple evolutions, specify which one you want on
the command line:

```-e <species>```

What you'll invariably find is that powering up existing pokemon is
always much better in the short term because it won't take much candy
to power them up to their next breakpoint(s) compared to evolving a
pokemon.  But a newly evolved pokemon may eventually surpass existing
pokemon if you spend enoough candy.

The power up breakpoints will be pretty obvious in the plot; they are
the large jumps in damage.  Between breakpoints, damage can increase
slowly, stay fairly level, or even decrease as your pokemon's ```(base
defense + defense IV) * CPM``` increases and it gains energy more slowly.

Damage can also be plotted vs. stardust cost by using ```-s``` instead
of ```-c```.  Remember that evolving takes no stardust so all
pokemon's plots start at zero stardust but it's going to cost some
candy first to get there if the pokemon is not already fully evolved.

DPS can be plotted instead of TDO by using ```-d``` instead of
```-t```.  DPS can really jump around a lot, both up and down, with
power ups.  You'll often find you get the best DPS by powering up only
to level 30-something.
