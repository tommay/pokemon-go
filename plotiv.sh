#!/bin/bash

set -e

# ./plotiv.sh attacker defender

# Makes a gnuplot script to create a color map (planview xyz plot) of
# matchup DPS and TDO while any two of level, attack, defense, and
# stamina are varied.
#
# The attacker and defender are given in my usual
# "species:LVL/ATK/DEF/STA[:QUICK/CHARGE] format, but two of LVL, ATK,
# DEF, and STA should be "%".  They will be varied along the X and Y
# axes of the plot.

# Uses "./plotiv" to generate the data then uses a here document with
# variable expansion to create the gnuplot script.

# ./plotiv.sh attacker defender | gnplot -persist
# gnuplot <(./plotiv rhydon:%/%/12/12:ms/surf vileplume:r5:a/sb)

# -p runs gnuplot automatically:

if [[ "$1" == "-p" ]]; then
  shift
  exec gnuplot <("$0" "$@")
fi

ATTACKER="$1"
DEFENDER="$2"

TITLE="${ATTACKER}\n${DEFENDER}"

if [[ "$ATTACKER" =~ :%/%/[0-9]+/[0-9]+ ]]; then
  X_LABEL="LVL"
  Y_LABEL="ATK"
elif [[ "$ATTACKER" =~ :%/[0-9]+/%/[0-9]+ ]]; then
  X_LABEL="LVL"
  Y_LABEL="DEF"
elif [[ "$ATTACKER" =~ :%/[0-9]+/[0-9]+/% ]]; then
  X_LABEL="LVL"
  Y_LABEL="STA"
elif [[ "$ATTACKER" =~ [0-9]+/%/%/[0-9]+ ]]; then
  X_LABEL="ATK"
  Y_LABEL="DEF"
elif [[ "$ATTACKER" =~ [0-9]+/%/[0-9]+/% ]]; then
  X_LABEL="ATK"
  Y_LABEL="STA"
elif [[ "$ATTACKER" =~ [0-9]+/[0-9]+/%/% ]]; then
  X_LABEL="DEF"
  Y_LABEL="STA"
else
  echo "Attacker needs IVs with two percent signs as placeholders."
  exit 1
fi

# Generate the plot data.

DATA=$(./plotiv "$ATTACKER" "$DEFENDER")

cat <<EOF
  # Plot xyz data with x across the bottom, y on the left, and z indicated
  # by color.

set view map
set size ratio 1.0

  # Black background.

set object 1 rect from graph 0, graph 0 to graph 1, graph 1 back
set object 1 rect fc rgb "black" fillstyle solid 1.0

  # I can't remember what was in the key but we don't need .

unset key

set xlabel "${X_LABEL}"
set ylabel "${Y_LABEL}"

  # Set up the colors to run hot -> cold: blue -> green -> yellow -> red.
  # Except I decided rainbow from blue to green works better.

set palette model RGB
set palette defined ( 0 "blue", 1 "magenta", 2 "red", 3 "yellow", 4 "green" )
#set palette defined ( 0 "red", 1 "yellow", 2 "green" )
#set palette defined ( 0 "#0000ff", 1 "#00ff00", 2 "#ffff00", 3 "#fff000" )
#set palette defined ( 0 "#000000", 1 "#ff0000", 2 "#ffff00", 3 "#ffffff" )

#set palette model HSV
#set palette defined ( 0 0 1 1, 1 1 1 1 )

set multiplot layout 1,2 \
  title "${TITLE}" font ",16"

set title "DPS" font ",16"
splot "-" using 1:2:3 with points \
   pointtype 5 pointsize 2 palette linewidth 30
${DATA}
e

set title "TDO" font ",16"
unset ylabel
splot "-" using 1:2:4 with points \
   pointtype 5 pointsize 2 palette linewidth 30
${DATA}
e

pause -1 "Hit return"
EOF
