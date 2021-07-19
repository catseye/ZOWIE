#!/bin/sh

PROG=zowie

if command -v ghc >/dev/null 2>&1; then
    echo "building $PROG.exe with ghc"
    (cd src && ghc --make Main.hs -o ../bin/$PROG.exe)
else
    echo "ghc not found, not building $PROG.exe"
fi

if command -v hastec-not-yet >/dev/null 2>&1; then
    echo "building $PROG.js with hastec"
    (cd src && hastec --make HasteMain.hs -o ../demo/$PROG.js) || exit 1
else
    echo "hastec not found, not building $PROG.js"
fi
