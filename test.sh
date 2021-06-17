#!/bin/sh

APPLIANCES="tests/appliances/zowie.py2.md tests/appliances/zowie.py3.md"
if [ ! `which rpython`X = X ]; then
    APPLIANCES="$APPLIANCES tests/appliances/zowie-c.md"
    if [ ! -e ./zowie-c ]; then
        ./build.sh || exit $?
    fi
fi

falderal $APPLIANCES tests/ZOWIE.md
