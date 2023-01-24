---
comments: true
---

## Accessing part of a structure

Lenses are the way to access and update parts of data structures in a pure functional way. Here are some examples using the `lens` package:

=== "Infix"

    ```hs title="repl example"
    > import Control.Lens

    > tuple = (True, ('a', 'b'))

    -- simple example: accessing the values inside a tuple
    > tuple ^. _1 -- (1)!
    True
    > tuple ^. _2
    ('a','b')
    > tuple ^. (_2 . _1) -- (2)!
    'a'

    -- updating parts of a data structure
    > tuple & _1 .~ 3 -- (3) (4)
    (3,('a','b'))
    > tuple & (_2 . _1) .~ 3
    (True,(3,'b'))
    > tuple & (_2 . _1) %~ toUpper
    (True,('A','b'))
    ```

    1. `_1` is a *lens*. It "points" towards a part of a data structure, in this case, the first element of a tuple. `^.` means: *get* the thing pointed to by the lens.
    2. `(_2 . _1)` is a lens too! It points towards the 1st element of the 2nd element of a tuple.
    3. Note: `tuple & _1 .~ 3` is a new tuple - it hasn't [mutated](/thinkingfunctionally/immutability/) the old one.
    4. `&` is just like `$` but in reverse: `x & f = f x`.

=== "Prefix"

    ```hs title="repl example"
    > tuple = (True, ('a', 'b'))
    > view _1 tuple
    True
    > view _2 tuple
    ('a','b')
    > view (_2 . _1) tuple
    'a'

    -- updating parts of a data structure
    > set _1  3 tuple
    (3,('a','b'))
    > set (_2 . _1) 3 tuple
    (True,(3,'b'))

    > over (_2 . _1) toUpper tuple
    (True,('A','b'))
    ```

## Updating part of a structure

You can also use the `lens` library to access or update multiple (or optional) parts of a data structure:

```hs
import Control.Lens

-- access multiple fields
> tuple = ((4, True), (3, False))
> tuple ^.. both -- (1)!
[(4,True),(3,False)]
> tuple ^.. both . _2 -- (2)!
[True,False]

-- update multiple values
> tuple & both . _2 %~ not
((4,False),(3,True))

-- example of an optional lookup
> ls = [(True, 'a'), (False, 'b'), (True, 'c')]

> ls ^.. ix 2 -- (3) (4)
[(True,'c')]

> ls ^.. ix 4
[]

-- an example with a dictionary, or Map
> import qualified Data.Map as M
> dictionary = M.fromList [("John", (True, 3)), ("Sally", (False, 1))]
> dictionary ^.. ix "John"
[(True,3)]
> dictionary ^.. ix "Jim"
[]
> dictionary ^.. (ix "John" . _1)
[True]

> dictionary & (ix "John" . _2) %~ (+1)
fromList [("John",(True,4)),("Sally",(False,1))]
```

1. `both` is a lens, or more precisely, a `Traversal`.
2. Like other lenses and traversals, `both` can be composed, here with `_2`.
3. `ix n` looks up the element at the nth index of a structure.
4. Since `ix n` may fail if `n` is out of bounds, it must be used with `^..`


!!! Note
    The `lens` package has two properties which make it unfriendly:

    1. A large set of packages it depends on.
    2. Sophisticated use of very abstract [typeclasses](/typeclasses/overview), which results in hard-to-interpret error messages:

    ```hs title="repl example"
    import Control.Lens
    > tuple = (True, ('a', 'b'))

    -- scary error message
    > tuple ^. (_1 . _1)
    "No instance for (Field1 Bool Bool () ())"

    -- another scary error message
    > tuple2 = (True, False)
    > tuple2 ^. both
    "No instance for (Monoid Bool) arising from a use of ‘both’"
    ```

    In either case, the error messages refer to typeclass constraints that your program requires, but without understanding the internals of the `lens` library, it is hard to make sense of these.

    The `optics` package aims to address these problems, with a different set of tradeoffs.


!!! Gotcha
    Haskell is [immutable](/thinkingfunctionally/immutability), so when we talk about changing a data structure, we mean producing a new data structure that has that change. For example, to change the third element of `[1,2,3]` to `4` is to produce a new list `[1,2,4]`.

## Custom data

Under :construction:

## Useful examples

The `lens` library has an enormous range of useful tools, some of which are not easily discoverable. Here are some examples:

### Set

```hs title="repl example"
> import qualified Data.Set as S
> numberSet = S.fromList [1,2,3,5]
> numberSet
fromList [1,2,3,5]
> numberSet ^. contains 3
True
> numberSet ^. contains 4
False
> numberSet & contains .~ 4

-- even update a set this way!
> numberSet & contains 4 .~ True
fromList [1,2,3,4,5]

> numberSet & contains 3 %~ not
fromList [1,2,5]
```

### Coercions 

```hs title="repl example"
> newtype Flag = Val Bool
> (Flag True & coerced %~ not) :: Bool -- (1)!
False
```

1. `coerced` is a lens, which points towards the `Bool` inside a `B`.

### Plated

Under :construction:

### iFoldMap

Under :construction: