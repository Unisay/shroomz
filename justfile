default:
  @just --list

# run dev derver
dev:
  #!/usr/bin/env bash
  cabal run &
  watchman-make -p '**/*.hs' '**/*.cabal' \
    -r 'clear; killall -q -s HUP shroomz cabal; cabal run &'
  wait

build:
  cabal build

