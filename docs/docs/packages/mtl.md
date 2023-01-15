# Under :construction:


In imperative programming languages, it is straightforward to write code which performs "side effects", like:

- throwing an error
- reading from a file
- reading from an environment variable
- writing to a log
- mutating a local variable

A standard way in Haskell to have these abilities in a pure functional setting is with the use of monads. 

`mtl`[^1] is a widely used library designed for combining effects like the above smoothly:


# Under :construction:

[^1]: Short for: monad transformer library

!!! Gotcha

    Alternatives to `mtl` have become popular in recent years, such as [Polysemy](todo) and [cleff](todo). These are often experimental, or require slightly more advanced use of types (e.g. [type level lists](todo)) and so are recommended for experienced Haskellers only.