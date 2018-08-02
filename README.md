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
