#!/bin/sh

if [ `which rpython`X = X ]; then
    echo 'RPython not found.  Not building.  Use CPython or Skulpt instead.'
else
    python `which rpython` src/zowie.py
    mkdir -p bin
    mv zowie-c bin/
fi
