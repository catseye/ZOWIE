#!/bin/sh

APPLIANCES=""
if command -v python2 > /dev/null 2>&1; then
    APPLIANCES="$APPLIANCES tests/appliances/zowie.py2.md"
fi
if command -v python3 > /dev/null 2>&1; then
    APPLIANCES="$APPLIANCES tests/appliances/zowie.py3.md"
fi
if [ -x bin/zowie-c ]; then
    APPLIANCES="$APPLIANCES tests/appliances/zowie-c.md"
fi
if [ -x impl/zowie-hs/bin/zowie-hs ]; then
    APPLIANCES="$APPLIANCES tests/appliances/zowie-hs.md"
fi

if [ "x$APPLIANCES" = "x" ]; then
    echo "No suitable Python versions or ZOWIE implementations found."
    exit 1
fi

falderal $APPLIANCES tests/ZOWIE.md
