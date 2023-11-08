```
     _____            _             
    |_   _|          (_)            
      | |  _ ____   ___ _ __   __ _     o    _______________
      | | | '__\ \ / / | '_ \ / _` |   /\_  _|             |
      | | | |   \ V /| | | | | (_| |  _\__`[_______________|
     _| |_|_|    \_/ |_|_| |_|\__, |  ] [ \, ][         ][
    |-----|                    __/ |
                              |___/
```

Irving is a tiny subset of [IVy](https://github.com/kenmcmil/ivy/), geared for
extracting protocols to other backends than C++.  Pairs great with
[Melina](https://github.com/dijkstracula/melina).

## Overview

IVy protocols, in addition to symbolic verification through bounded model
checking, can be compiled into an executable binary by way of extraction to
C++.  These extracted programs can be driven manually via a built-in REPL or
automatically with random inputs derived from the protocol's specification.

At the moment, Irving is intended to be simply a parser for a small but
hopefully reasonable subset of IVy 1.8, a typechecker, and an associated AST
transformer (notably, to extract the Ivy program to other languages).  The
intention is that IVy itself can be used for model-checking during protocol
development and then Irving can more flexibly extract to a desired runtime,
like Melina for Java protocols.  (It would be fun to implement a bit of the
model checking stuff in Irving in order to better elucidate the technique, but
that's out of scope for the moment.)

## Development

Irving uses `pre-commit` to ensure that committed code is formatted and
passes tests.  Install `pre-commit` via your favourite package manager and
hook the hook with:

```
$ pre-commit install
```

If you want to manually run the pre-commit (on committed files), run:

```
$ pre-commit run
```

## Running

```
$  RUST_LOG=info,sort-substituter=debug,visitor=trace \ 
  cargo run programs/200_chainrep.ivy extract ivy
```

## Testing

Ensure you've checked out submodules - The Ivy implementation of the
[Accord](https://github.com/dijkstracula/accord-ivy/) consensus protocol is
part of the test suite.  So, be sure to add `--recurse-submodules` to `git
init` and `git pull`.
