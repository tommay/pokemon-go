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
  evolved="-e $2"
  shift; shift
fi

species=${1%%.*}           # Extract just the species from species.team.
species=${species##*/}     # Remove any directory.

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

rate -g <$1 >$1.great
rate -u <$1 >$1.ultra
rate -m <$1 >$1.master
