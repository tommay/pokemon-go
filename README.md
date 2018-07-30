# pokemon battle simulator

This is my own pokemon battle simulator.  Written in Haskell which I
wish I'd learned a long time ago.

This README is just a brief overview.

The calculations and defender's move selection are based on (IIRC)
Silph Road battle analysis posts.  Dodging is not supported.  I never
dodge because a) I don't see well enough and b) dodging lowers DPS.

I'm not sure how exceedingly accurate it is but I ve been using it to
help choose raid counters for a long time now and I'm never
disappointed.

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

