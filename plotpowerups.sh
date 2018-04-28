#!/bin/bash

set -e

# ./plotpowerups.sh [-p] [-t] -f file defender

# Makes a gnuplot script to create a plot of dps vs. candy cost.

# Uses "./plotpowerups" to generate the data then uses a here document with
# variable expansion to create the gnuplot script.

# ./plotiv.sh attacker defender | gnuplot -persist
# gnuplot <(./plotiv rhydon:%/%/12/12:ms/surf vileplume:r5:a/sb)

# -p runs gnuplot automatically:

if [[ "$1" == "-p" ]]; then
  shift
  exec gnuplot -persist <("$0" "$@")
fi

# -t plots tdo instead of dps:

if [[ "$1" == "-t" ]]; then
  shift
  yfield=4
else
  yfield=3
fi

# Generate the plot data.

DATA=$(./plotpowerups "$@")

cat <<EOF
set ylabel "DPS"
set y2label "TDO"

# https://stackoverflow.com/questions/12818797/how-to-plot-several-datasets-with-titles-from-one-file-in-gnuplot

set key right bottom
plot for [i=0:*] "-" using 1:$yfield \\
  with linespoints lw 3 pt 7 title columnheader(1)
${DATA}

pause -1 "Hit return"
EOF
