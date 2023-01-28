Don't confuse values and types. 

For example, the *type* `Pair`

```hs
data Pair a = P a a 
```

takes **one** type parameter, `a`, but the *value* `P` takes **two** arguments of the type `a`.

```hs
example :: Pair Bool
example = P True False 
```

The danger of confusion is exacerbated by [punning](/gotchas/punning/).


