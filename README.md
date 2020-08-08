# VB6-optim

This is an 🚧 **extremely alpha-stage** 🚧 piece of software.  It
works for certain narrow use cases the authors have, and might be
interesting to you, but it's very likely to explode in your face, or
return a clown instead of an integer.

## Wat do

For the impatient - this package uses
[Stack](https://docs.haskellstack.org/en/stable/README/).  You'll
probably want to `brew install haskell-stack`.

```shellsession
$ stack install alex happy
$ make
# you can also run tests:
$ make test
```

## What even is this?

We had a desire to write a [ray
tracer](https://github.com/visual2000/BasicTrace) in [Visual Basic
6](https://en.wikipedia.org/wiki/Visual_Basic), the easiest way to
create a retro GUI.  The thing is, performance was awful.  So we did
[some benchmarks](https://github.com/visual2000/Microbenchmarks) and
thought we could probably reshuffle our code.  It was very
function-call-heavy of course, and in VB that's pretty slow.
Unfortunately it turned out to be quite a rabbit hole, and so we first
had to write a parser, then a pretty printer, then a bunch of
optimisation phases, all of which were a bit more involved than we'd
hoped.
