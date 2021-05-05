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

## Checking TodoMVC

To check a TodoMVC implementation from the command line, use a command
like this:

```
poetry run quickstrom check todomvc $TODOMVC_DIR/examples/angularjs/index.html -I ulib -I case-studies
```
