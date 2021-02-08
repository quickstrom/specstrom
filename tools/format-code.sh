#!/usr/bin/env bash

find . -not -path './dist-newstyle/*' -name '*.hs' -exec ormolu -m inplace {} \;