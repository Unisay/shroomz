default:
  just --list

# run dev derver
dev:
  ghcid --command 'cabal repl' --test 'Shroomz.Dev.update'
