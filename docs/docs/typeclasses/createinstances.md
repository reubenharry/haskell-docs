---
comments: true
---

!!! Warning 

    It is recommended that you avoid creating your own type classes unless it is entirely necessary.

    They are not intended to be used in the same way as Python classes, for example. 

To create a typeclass:

```hs
class Addable a where
    add :: a -> a -> a
```

!!! Warning
    This isn't a type class that is necessary to create, since the `Num` type class already does something similar.

```hs
instance Addable Int where
    add = (+)
```

### Type class recursion

```hs
instance Addable (Int, Int) where
    add (x, y) (x', y') = (add x x', add y y')
```

Here, `add` on the right hand side of the definition is the `add` method for

> add (4 :: Int) 5
9
```