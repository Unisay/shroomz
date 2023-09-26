default:
  @just --list

# run dev derver for the target app
dev target='shroomz-demo':
  #!/usr/bin/env bash
  cabal run {{target}} &
  watchman-make -p '**/*.hs' '**/*.cabal' \
    -r 'clear; killall -q -s HUP {{target}} cabal; cabal run {{target}} &'
  wait

build:
  cabal build shroomz-demo

