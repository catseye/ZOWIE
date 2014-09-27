#!/bin/sh

FIXTURES=''
if [ ! `which rpython`X = X ]; then
    FIXTURES="$FIXTURES tests/rpython-fixture.markdown"
    if [ ! -e ./zowie-c ]; then
        ./build.sh || exit $?
    fi
fi

falderal --substring-error $FIXTURES tests/ZOWIE.markdown
