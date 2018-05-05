#!/bin/bash

set -e

# ./plotpowerups.sh [-p] [-t] -f file defender

# Makes a gnuplot script to create a plot of dps vs. candy cost.

# Uses "./plotpowerups" to generate the data then uses a here document with
# variable expansion to create the gnuplot script.

# ./plotiv.sh attacker defender | gnuplot -persist
# gnuplot <(./plotiv rhydon:%/%/12/12:ms/surf vileplume:r5:a/sb)

# -p runs gnuplot automatically.

if [[ "$1" == "-p" ]]; then
  shift
  exec gnuplot -persist <("$0" "$@")
fi

# Generate the plot data.

DATA=$(./plotpowerups "$@")

cat <<EOF
set title "$*" offset 0,-2.5
set ylabel "$ylabel"
set grid ytics lt 1

# https://stackoverflow.com/questions/12818797/how-to-plot-several-datasets-with-titles-from-one-file-in-gnuplot

set key right bottom

${DATA}

EOF
