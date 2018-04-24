#!/bin/bash

set -e

# ./plotpowerups.sh -f file defender

# Makes a gnuplot script to create a plot of dps vs. candy cost.

# Uses "./plotpowerups" to generate the data then uses a here document with
# variable expansion to create the gnuplot script.

# ./plotiv.sh attacker defender | gnuplot -persist
# gnuplot <(./plotiv rhydon:%/%/12/12:ms/surf vileplume:r5:a/sb)

# -p runs gnuplot automatically:

if [[ "$1" == "-p" ]]; then
  shift
  exec gnuplot <("$0" "$@")
fi

# Generate the plot data.

DATA=$(./plotpowerups "$@")

cat <<EOF
set ylabel "DPS"
set y2label "TDO"

# https://stackoverflow.com/questions/12818797/how-to-plot-several-datasets-with-titles-from-one-file-in-gnuplot

set key right bottom
plot for [i=0:*] "-" index i using 1:3 \
  with lines lw 3 title columnheader(1)
${DATA}
e

pause -1 "Hit return"
EOF
