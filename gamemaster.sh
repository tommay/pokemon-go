#!/bin/sh

# curl -O https://raw.githubusercontent.com/pokemongo-dev-contrib/pokemongo-game-master/master/versions/latest/GAME_MASTER.json

curl https://raw.githubusercontent.com/ZeChrales/PogoAssets/master/gamemaster/gamemaster.json >GAME_MASTER.json

json2yaml.rb GAME_MASTER.json >GAME_MASTER.yaml

patch --backup <legacy_moves.patch && \
  diff -u GAME_MASTER.yaml.orig GAME_MASTER.yaml >legacy_moves.patch
