#!/bin/sh

# rate.sh species.team
#
# Used to help decide which pokemon to keep go pvp.
#
# Reads stats from file species.team and creates species.team.great
# and species.team.ultra.  Then use "best [-a] species.team.great" or
# "best [-a] species.team.*" to sort by stat product or attack and
# decide which pokemon to keep.

species=${1%%.*}           # Extract just the species from species.team.
species=${species##*/}     # Remove any directory.

while read stats; do
  # If the stats line starts with a letter then it already includes
  # the species, else prepend the species.
  if [[ "$stats" != [a-z]* ]]; then
    stats="$species $stats"
  fi
  ./bulk -s -g $stats
done <$1 | egrep -v "too high" >$1.great

while read stats; do
  if [[ "$stats" != [a-z]* ]]; then
    stats="$species $stats"
  fi
  ./bulk -s -u $stats
done <$1 | egrep -v "too high" >$1.ultra


