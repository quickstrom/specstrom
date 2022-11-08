# Specstrom

[![Build and test](https://github.com/quickstrom/specstrom/actions/workflows/test.yml/badge.svg)](https://github.com/quickstrom/specstrom/actions/workflows/test.yml)

## Running tests

The regular Cabal way runs all tests:

```
cabal test
```

But you can also use `ghcid` to watch files and rerun tests on changes:

```
chmod go-w .ghci
HEDGEHOG_COLOR=1 ghcid test/Main.hs --test main
```
