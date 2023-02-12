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

```hs title="repl example"
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

You can write lenses for custom types, or generate them automatically with [Template Haskell](/resources/articles/#template-haskell-metaprogramming) (a macro extension to Haskell):

```haskell
{-# LANGUAGE TemplateHaskell #-} 
import Control.Lens

data Point a
  = Point { _x :: a, _y :: a }
makeLenses ''Point -- (1)!

example = Point {_x = 2, _y = 3}

xVal = example ^. x
yVal = example ^. y

main = print xVal
```

1. This is the syntax for a Template Haskell macro, here `makeLenses`, which `Control.Lens` exports.

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

### JSON

Lenses are particularly useful for working with JSON data, and can automate almost any querying or updating task, even very complex ones.

Let's use this JSON as an example, saved in `"data/file.json"`:

```json
{
  "firstName": "John",
  "lastName": "Smith",
  "isAlive": true,
  "age": 27,
  "address": {
    "streetAddress": "21 2nd Street",
    "city": "New York",
    "state": "NY",
    "postalCode": "10021-3100"
  },
  "phoneNumbers": [
    {
      "type": "home",
      "number": "212 555-1234"
    },
    {
      "type": "office",
      "number": "646 555-4567"
    }
  ],
  "children": [
      "Catherine",
      "Thomas",
      "Trevor"
  ],
  "spouse": null
}
```

```hs title="repl example"
-- read into a string
> json <- readFile "data/file.json"
> json
"{\n    \"firstName\": \"John\",\n    \"lastName\": \"Smith\",\n    \"isAlive\": true,\n    \"age\": 27,\n    \"address\": {\n      \"streetAddress\": \"21 2nd Street\",\n      \"city\": \"New York\",\n      \"state\": \"NY\",\n      \"postalCode\": \"10021-3100\"\n    },\n    \"phoneNumbers\": [\n      {\n        \"type\": \"home\",\n        \"number\": \"212 555-1234\"\n      },\n      {\n        \"type\": \"office\",\n        \"number\": \"646 555-4567\"\n      }\n    ],\n    \"children\": [\n        \"Catherine\",\n        \"Thomas\",\n        \"Trevor\"\n    ],\n    \"spouse\": null\n  }"

-- imports
> import Data.Aeson
> import Data.Aeson.Lens
> import Text.Pretty.Simple
> import Control.Lens

-- view the underlying JSON data structure as a Haskell value "inside" the string:
> pPrint $ json ^.. _Value -- (3)!
[ Object
    ( fromList
        [
            ( "address"
            , Object
                ( fromList
                    [
                        ( "city"
                        , String "New York"
                        )
                    ,
                        ( "postalCode"
                        , String "10021-3100"
                        )
                    ,
                        ( "state"
                        , String "NY"
                        )
                    ,
                        ( "streetAddress"
                        , String "21 2nd Street"
                        )
                    ]
                )
            )
...

> query (_Value . key "address" . key "city") -- (2)!
[ String "New York" ]

> query (_Value . key "children" . nth 2)
[ String "Trevor" ]

> query (_Value . key "children" . nth 1)
[ String "Thomas" ]

> query (_Value . key "spouse" . nth 1) -- (4)!
[]

> query (_Value . key "children" . values)
[ String "Catherine"
, String "Thomas"
, String "Trevor"
]

```


1. This is a Haskell data structure (of type `Value` from `Data.Aeson`) which represents the JSON. 
2. As usual, we can compose lenses
3. `pPrint` is a pretty printing function. The *lens* (or more precisely, the *traversal*) is `_Value`.
4. Since there is no list inside the entry for "spouse", this returns no results.

The most powerful use cases involve recursively searching the JSON, using a lens which (lazily) points to every JSON subpart in the JSON as a whole:


```hs title="repl example"

-- for all Strings in the JSON, show them
> pPrint $ json ^.. _Value . cosmos . _String
[ "New York"
, "10021-3100"
, "NY"
, "21 2nd Street"
, "Catherine"
, "Thomas"
, "Trevor"
, "John"
, "Smith"
, "212 555-1234"
, "home"
, "646 555-4567"
, "office"
]

-- for all Strings in the JSON underneath the key "address", show them:
> pPrint $ json ^.. _Value . key "address" . cosmos . _String
[ "New York"
, "10021-3100"
, "NY"
, "21 2nd Street"
]

-- for all arrays in the JSON that contain at least 3 elements, show the third:
> pPrint $ json ^.. _Value . cosmos . _Array . ix 2
[ String "Trevor" ]

```

One can similarly update the raw JSON string in a structured way with lenses:

```hs title="repl example"
-- modify the string corresponding to the address's city to be uppercase
> json & _JSON' @String @Value . key "address" . key "city" . _String %~ T.map toUpper
"{\"address\":{\"city\":\"NEW YORK\",\"postalCode\":\"10021-3100\",\"state\":\"NY\",\"streetAddress\":\"21 2nd Street\"},\"age\":27,\"children\":[\"Catherine\",\"Thomas\",\"Trevor\"],\"firstName\":\"John\",\"isAlive\":true,\"lastName\":\"Smith\",\"phoneNumbers\":[{\"number\":\"212 555-1234\",\"type\":\"home\"},{\"number\":\"646 555-4567\",\"type\":\"office\"}],\"spouse\":null}"
```
