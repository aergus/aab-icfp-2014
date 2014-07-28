# AAB @ ICFP Programming Contest 2014

This code was written by the team "AAB" for the
[ICFP programming contest](http://icfpcontest.org) of 2014.

## Code

The project consists of several submodules within the Haskell module `AAB`:

 * `GCC` (and its respective submodules) contain code for emulating and
generating Lambda-Man code.
 * `GHC` contains code for emulating and generating ghost code.
 * `Game` contains code for emulating the game.

In addition to the Haskell code, the directory `strategies` contains specific
ghost and Lambda-Man code files.

### GCC

`GCC.Step` emulates Lambda-Man code. We didn't get to implement the several
language extensions here.

`GCC.Base` contains functions to run the code, either step-by step with
comments, or directly. Here we already implemented part of a function of
the following signature:
```
Code -> (Worldstate -> Ghostcode -> (AIState, AIState -> Worldstate -> (AIState, Int)))
```
However, since the `Game` module never got finished, we didn't remove the
placeholders.

`GCC.Testscript` and `GCC.Parser` allow to test-run code from file.

One of the files compiled with `mk.sh` is `testgcc`, this allows to step-by-step
test code from file. The other files compiled by `mk.sh` allow to compile
Lambda-Man code from our LISP dialect. It is quite standard. The notable twists
are:

 * named toplevel expressions, allowing easy recursion
 * inline recursive functions, syntax `(\r f a1 ... an -> exp)`, where `f` is
the name of the function, to be used in the body `exp`
 * "do-notation": `{ x1 <- e1; ...; xn <- en; exp}` can be used to elegantly
bind variables. Here `xi` can be used in all further expressions, and in the
body. This is very useful, because e.g. `{x <- e1; (f x x)}` is translated to
`((\ x -> (f x x)) e1)`, where `e1` has to be evaluated only once (as opposed to
twice in `(f e1 e1)`), which increases performance

### GHC

`GHC.Pipeline` can be used to assemble ghc code.

### Game

Sadly unfinished.

### Strategies

`ghosts`: These have to be assembled with Ghc.

`lambdamen`: These have to be compiled with `ll2gcc` (or a variant of it). We
used Unix pipes and /dev/stdin to manually handle imports, for example:
```
cat lambdaSophisticated.lisp dfs.lisp lambdapi.lisp helper.lisp | ../../ll2gcc /dev/stdin
```
