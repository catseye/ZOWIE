#!/bin/sh

APPLIANCES="tests/appliances/zowie.py2.md tests/appliances/zowie.py3.md"
if [ -x bin/zowie-c ]; then
    APPLIANCES="$APPLIANCES tests/appliances/zowie-c.md"
fi

falderal $APPLIANCES tests/ZOWIE.md
