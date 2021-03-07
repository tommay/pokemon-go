#!/bin/bash

# Sorts all pokemon+moveset first by spamminess (how many turns it
# takes for them to do a change move, lowest first) and secondarily by
# how much damage they do adjusted for the moves' STAB.
#
# The idea is that spammy moves are good because it's not a big deal
# if they get shielded, but they also need to put out respectable
# damage.
#
# This script currently takes about 6 minutes to run.

# Use tankiness to get a list of all pokemon that have good enough cp
# for great league.

all_pokemon=$(./tankiness | awk '{ print $1 }')
for p in $all_pokemon; do
  # XXX Should have a switch to filter types for various cups.
  if true || ./effectivenessagainst $p | egrep -q "type: .*flying"; then
    ./spam $p |
      awk -F: '{ print $2, $1 }' |
      sed -e "s/^ *//" -e "s/ *$/ $p/"
  fi
done | sort -k 1n -k 2nr
