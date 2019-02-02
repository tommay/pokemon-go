#!/bin/sh

curl -O https://raw.githubusercontent.com/pokemongo-dev-contrib/pokemongo-game-master/master/versions/latest/GAME_MASTER.json

json2yaml.rb GAME_MASTER.json >GAME_MASTER.yaml
