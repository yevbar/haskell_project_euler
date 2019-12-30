# Project Euler

## Origin

I had two things I wanted to work on:

* Solving [Project Euler](https://projecteuler.net) problems [PE is an awesome collection of coding/math problems!]
* Learning [Haskell](https://www.haskell.org)

So I decided to bundle both into a new thing to take on!

## Structure

As of right now, I simply put my work on problem `X` into a file named `X.hs`

## Running

Firstly, you'll need the [Glasgow Haskell Compiler](https://www.haskell.org/ghc/) installed (this is available through most package managers)

Once you've set that up, trying out one of my solution is a matter of compiling and running a file

```bash
$ ghc 4.hs
$ ./4
906609
```

To clean up the directory of non-source files, I've included a Makefile with a clean script

```bash
$ make clean
```
