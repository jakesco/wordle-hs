# Wordle.hs
Play Wordle on the command line.

Thanks to Josh Wardle for creating the original game: https://www.powerlanguage.co.uk/wordle/

![wordls-hs-demo](https://user-images.githubusercontent.com/43647018/153726753-999e3c5a-b22e-4f38-881e-ba7a1461d2c0.gif)

## Docker Image

A docker image is available on Docker Hub. If you have Docker installed run:
```bash
$ docker run -it jakesco/wordle-hs
```

## Pre-compiled Binaries

The [Releases](https://github.com/jakesco/wordle-hs/releases) page provides pre-compiled binaries for Linux and Windows 64-bit systems. Simply copy the desired binary into your path and run it (after verifying the checksum).

> Windows command prompt doesn't support ANSI escape sequences used to color the Wordle.hs output. Use
> [Windows Terminal](https://github.com/microsoft/terminal) for the best expierience.

## Manual Install

The recommend installation method is using `cabal-install`. Most linux distribuitons have `cabal` and `ghc` available in the official repositories. Alternatively you can install them on any platform using [ghcup](https://www.haskell.org/ghcup/).

This project requires `GHC 8.10`.

Build with `cabal-install`:
```bash
$ cabal update
$ cabal build
```

Then you can run from the project directory with:
```bash
$ cabal run wordlehs
```

or install the executable.
```bash
$ cabal install
$ wordlehs
```

> The default install target for `cabal install` is `~/.cabal/bin`. Make sure the `wordlehs` executable is on your path.
