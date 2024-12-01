default:
    @just --list

# Run hoogle
docs:
    echo http://127.0.0.1:8888
    hoogle serve -p 8888 --local

# Run cabal repl
repl *ARGS:
    cabal repl {{ ARGS }}

# Run ghcid -- auto-recompile and run `main` function
run:
    ghcid -T :main

here:
  echo sudo systemctl stop nickname-bot.service | ssh tub
  cabal run

there:
  echo sudo systemctl start nickname-bot.service | ssh tub

