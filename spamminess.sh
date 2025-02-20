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

league=-g

while getopts "gluc:k" opt; do
  case ${opt} in
    g) 
      league=-g
      ;;
    l) 
      league=--little
      ;;
    u) 
      league=-u
      ;;
    c)
      cup="-c $OPTARG"
      ;;
    k)
      classic=-c
      ;;
  esac
done

league="$league $classic"

all_pokemon=$(./list $cup)
for p in $all_pokemon; do
  ./spam $p $league -s -r |
    awk -F: '{ print $2, $1 }' |
    sed -e "s/^ *//" -e "s/ *$/ $p/"
done | sort -k 1n -k 2nr
