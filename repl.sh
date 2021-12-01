#! zsh

idris2 --build Advent.ipkg 
idris2 src/Main.idr -o advent

rlwrap idris2 ./src/Main.idr
