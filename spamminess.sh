#!/bin/bash

# Sorts all pokemon+moveset first by spamminess (how many turns it
# takes for them to do a charge move, lowest first) and secondarily by
# how much damage they do adjusted for the moves' STAB and the attacker's
# stat product.
#
# The idea is that spammy moves are good because it's not a big deal
# if they get shielded, but they also need to put out respectable
# damage.
#
# This script currently takes about 6 minutes to run.

all_pokemon=$(./list "$@")
for p in $all_pokemon; do
  ./spam $p --little -s |
    awk -F: '{ print $2, $1 }' |
    sed -e "s/^ *//" -e "s/ *$/ $p/"
done | sort -k 1n -k 2nr
