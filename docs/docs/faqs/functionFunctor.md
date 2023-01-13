---
comments: true
---

What is this code doing?

```hs title="repl example"
> func = fmap not (\x -> x > 3) 
> func 2
True
> func 4 
False
```

## Answer

`fmap` is the method of the [Functor](/typeclasses/survey/#functor) [typeclass](/typeclasses/overview).

To understand what `fmap` does whenever it is called, it is necessary to know which *instance* of `fmap` is being used. You can find this out by mousing over `fmap` (first place the line `func = fmap not (\x -> x > 3)` in a Haskell file in your project), to see:

![Functor](/img/functor.png)

The line `$dFunctor :: Functor ((->) Integer)` means that the instance of `Functor` being used is `((->) Integer)`.

!!! Tip
    If it is unclear what `((->) Integer)` means, see [here](/basics/functions/#partial-application-for-types) and [here](/basics/syntax/#infixing-in-types).



# Under :construction: 

    