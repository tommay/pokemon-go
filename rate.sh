#!/bin/sh

# rate.sh [-e evolved_species] species.team
#
# Used to help decide which pokemon to keep go pvp.
#
# Reads stats from file species.team and creates species.team.great
# and species.team.ultra.  Then use "best [-a] species.team.great" or
# "best [-a] species.team.*" to sort by stat product or attack and
# decide which pokemon to keep.

if [[ "$1" == "-e" ]]; then
  evolved_species="$2"
  evolved="-e $evolved_species"
  shift; shift
else
  evolved_species=$(./evolvecp $species | tail -n 1 | sed "s/:.*//")
fi

dir=${1%/*}                 # Remove filename, keep directory.
file=${1##*/}               # Remove directory, keep filename.

species=${file%.*}          # Remove team, keep species from species.team.
team=${file#*.}             # Remove species, keep team from species.team.

rate() {
  league="$1"
  while read stats; do
    # If the stats line starts with a letter then it already includes
    # the species, else prepend the species.
    if [[ "$stats" != [a-z]* ]]; then
      stats="$species $stats"
    fi
    echo $stats >&2
    ./bulk -s $evolved $league $stats
  done | egrep -v "too high"
}

out=$dir/$evolved_species.$team
rate -g <$1 >$out.great
rate -u <$1 >$out.ultra
rate -m <$1 >$out.master
