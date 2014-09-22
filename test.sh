#!/bin/sh

RPYTHON_FIXTURE=''
if [ ! `which rpython`X = X ]; then
    if [ `which under-pty`X = X ]; then
        echo 'Need the under-pty utility to propertly test rpython-compiled version.'
        exit 1
    fi
    RPYTHON_FIXTURE='tests/rpython-fixture.markdown'
    if [ ! -e ./zowie-c ]; then
        ./build.sh || exit $?
    fi
fi

falderal --substring-error $RPYTHON_FIXTURE tests/ZOWIE.markdown
