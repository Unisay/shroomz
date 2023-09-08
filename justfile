default:
  @just --list

# run dev derver
dev:
  #!/usr/bin/env bash
  cabal run shroomz-demo &
  watchman-make -p '**/*.hs' '**/*.cabal' \
    -r 'clear; killall -q -s HUP shroomz-demo cabal; cabal run shroomz-demo &'
  wait

build:
  cabal build shroomz-demo

