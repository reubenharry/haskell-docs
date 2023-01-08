Typeclasses add constraints to polymorphic types:

```haskell
notEqual :: Eq a => (a, a) -> Bool
notEqual (x,y) = not (x == y)
```

The type of `notEqual` states: for any type `a` **such that `a` is an instance of the `Eq` typeclass**, give me a pair of `a`s and I will return a `Bool`.

!!! Note
    `Eq a` is not a type, but rather a different *kind* of entity, called a constraint.

    `Eq` is referred to as a typeclass.

### Classes

todo:
    example of a class

    some number of methods

    how to find

    common classes include: `Show`, `Eq`, `Ord`, `Num`, `Monoid`, `Foldable`, `Functor` and `Monad`

### Constraints propagate

```hs
complexFunction :: Eq a => a -> ...
complexFunction x = let y = notEqual (x, x) in ...
```

Because `notEqual` is called on `(x, x)`, `x` must be of a type that is an instance of `Eq`. 

The compiler will reason in this way, even if you don't write a type signature.

### Inheritance

Semigroup a => Monoid a

todo 

### Higher kinded type classes

todo