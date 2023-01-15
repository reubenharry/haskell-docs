---
comments: true
---

# Under :construction:

`lens` is a package for accessing and updating deeply nested data structures in a pure functional way.

!!! Note
    The `lens` package has two properties which make it unfriendly:

    1. A large set of packages it depends on.
    2. Sophisticated use of very abstract [typeclasses](/typeclasses/overview), which can result in near-unreadable errors.

    The `optics` package aims to address these problems, with a different set of tradeoffs.


## Viewing a field

Under :construction:


<!-- tuple access, list access, map access, composition -->

## Viewing optional fields

## Viewing multiple fields
Under :construction:

## Changing fields

Under :construction:

!!! Gotcha
    Haskell is [immutable](/thinkingfunctionally/immutability), so when we talk about changing a data structure, we mean producing a new data structure that has that change. For example, to change the third element of `[1,2,3]` to `4` is to produce a new list `[1,2,4]`.