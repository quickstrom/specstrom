# Specstrom

## Running tests

The regular Cabal way runs all tests:

```
cabal test
```

But you can also use `ghcid` to watch files and rerun tests on changes:

```
ghcid test/Main.hs --test main
```