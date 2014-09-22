#!/bin/sh

if [ `which rpython`X = X ]; then
    echo 'RPython not found.  Not building.  Use CPython or Skulpt instead.'
else
    rpython src/zowie.py
fi
