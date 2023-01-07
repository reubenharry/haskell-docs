
```haskell
foo = 
    let bar = 5 
        foo = bar + 5
    in bar + foo + 1 
```

Let-bindings may be recursive: todo