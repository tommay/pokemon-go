#!/bin/sh

if [ "$1" == "" ]; then
  branch=master
else
  branch="$1"
fi

curl -O https://raw.githubusercontent.com/pokemongo-dev-contrib/pokemongo-game-master/$branch/versions/latest/GAME_MASTER.json

json2yaml.rb GAME_MASTER.json >GAME_MASTER.yaml
