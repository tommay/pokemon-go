#!/bin/sh

# rate.sh [-e evolved_species] [-c max_candy] [-x max_xl_candy] species.team
#
# Used to help decide which pokemon to keep go pvp.
#
# Reads stats from file species.team and creates species.team.great
# and species.team.ultra.  Then use "best [-a] species.team.great" or
# "best [-a] species.team.*" to sort by stat product or attack and
# decide which pokemon to keep.

# Default is to rate for al three leagues.

great=t
ultra=t
master=t
little=

while getopts ":e:c:x:lgum" opt; do
  case ${opt} in
    e) 
      evolved_species="$OPTARG"
      evolved="-e $evolved_species"
      ;;
    c)
      max_candy="-c $OPTARG"
      ;;
    x)
      max_xl_candy="-x $OPTARG"
      ;;
    g)
      great=t; ultra=; master=; little=
      ;;
    u)
      great=; ultra=t; master=; little=
      ;;
    m)
      great=; ultra=; master=t; little=
      ;;
    l)
      great=; ultra=; master=; little=t
      ;;
    \?)
      echo "Usage: $0 [-e evolved_species] [-c max_candy] [-x max_xl_candy] [-g] [-u] [-m]"
      exit 2
      ;;
  esac
done

shift $((OPTIND-1))

dir=${1%/*}                 # Remove filename, keep directory.
file=${1##*/}               # Remove directory, keep filename.

species=${file%.*}          # Remove team, keep species from species.team.
team=${file#*.}             # Remove species, keep team from species.team.

if [[ "$evolved_species" == "" ]]; then
  if [[ "$little" ]]; then
    evolved_species="$species"
  else
    evolved_species=$(./evolvecp $species | tail -n 1 | sed "s/:.*//")
  fi
fi

rate() {
  league="$1"
  while read stats; do
    # If the line is a comment then ignore it.
    if [[ "$stats" != \#* ]]; then
      # If the stats line starts with a letter then it already includes
      # the species, else prepend the species.
      if [[ "$stats" != [a-z]* ]]; then
        stats="$species $stats"
      fi
      echo $stats >&2
      ./bulk -s $max_candy $max_xl_candy $evolved $league $stats
    fi
  done | egrep -v "too high"
}

out=$dir/$evolved_species.$team
[ "$great" ] && rate -g <$1 >$out.great
[ "$ultra" ] && rate -u <$1 >$out.ultra
[ "$master" ] && rate -m <$1 >$out.master
[ "$little" ] && rate --little <$1 >$out.little
