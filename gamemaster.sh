#!/bin/sh

if [ "$1" == "" ]; then
  branch=master
else
  branch="$1"
fi

curl -O https://raw.githubusercontent.com/PokeMiners/game_masters/$branch/latest/latest.json

json2yaml.rb latest.json >GAME_MASTER.yaml && rm latest.json
