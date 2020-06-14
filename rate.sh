#!/bin/sh

# rate.sh species.team
#
# Used to help decide which pokemon to keep go pvp.
#
# Reads stats from file species.team and creates species.team.great
# and species.team.ultra.  Then use "best [-a] species.team.great" or
# "best [-a] species.team.*" to sort by stat product or attack and
# decide which pokemon to keep.

species=${1%%.*}           # Extract justthe species from species.team.
species=${species##*/}     # Remove any directory.

while read stats; do
  ./bulk -s -g $species $stats
done <$1 | egrep -v "too high" >$1.great

while read stats; do
  ./bulk -s -u $species $stats
done <$1 | egrep -v "too high" >$1.ultra


