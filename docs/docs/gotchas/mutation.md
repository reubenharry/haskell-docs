"I wrote a simple piece of code to update the value of `x`, but it hangs."

=== "In a repl"

    ```haskell
    x = 0
    x = x + 1
    x
    ```

=== "In a file"

    ```haskell
    x = 0
    x = x + 1
    main = print x
    ```

In Haskell, [values are always immutable](/thinkingfunctionally/immutability). Writing `a = b` does not mean "set the value of `a` to the current value of `b`". It means "bind the name `a` to the expression `b`. So you have asked Haskell to make `x` refer to the value `x + 1`. 

Haskell tries to fulfill your request. When you ask for `x` on the third line, it looks up its value, which is `x + 1`. It then looks up the value of `x` in `x + 1`, which again, is `x + 1`, to obtain `(x + 1) + 1`. This continues indefinitely, which is why the program hangs. 
