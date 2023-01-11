```hs title="repl example"
> func = fmap not (\x -> x > 3) 
> func 2
True
> func 4 
False
```

What is this code doing?

todo 

    cases like this, one may know from the type of `fmap` that its second argument should be of the form `f a` where `f` is a `Functor` instance
    
    one should identify the typeclass in question, (here the typeclass is `Functor` 

    