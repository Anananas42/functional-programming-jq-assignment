# Project for FP course at TU Delft

A clone of `jq` in Haskell.

You will find the description of the task on Brightspace.

## Build
```
> stack build
```

## Install

```
> stack install
```

this installs your executable to `~/.local/bin` by default (on *nix), make sure it's in $PATH

## Test

```
stack test
```

You will need `jq` installed and available on `$PATH` to run `from-upstream` test suite.

## Use

```
> echo '{"this" : "that"}' | jq-clone '.this'
```

or

```
> echo '{"this" : "that"}' | stack run -- '.this'
```

## Test `jq` online

[jqplay.org](https://jqplay.org/)

## Docs

[jqlang.github.io/jq/manual](https://jqlang.github.io/jq/manual/)
# functional-programming-jq-assignment
