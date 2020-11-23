#!/bin/sh

if [ "$1" == "" ]; then
  branch=master
else
  branch="$1"
fi

curl -O https://raw.githubusercontent.com/pokemongo-dev-contrib/pokemongo-game-master/$branch/versions/latest/V2_GAME_MASTER.json

json2yaml.rb V2_GAME_MASTER.json >V2_GAME_MASTER.yaml && rm V2_GAME_MASTER.json
