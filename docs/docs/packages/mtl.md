!!! Warning

    Understand monads, do-notation, and in particular the `State` and `Except` monads in advance will make the content of this page more transparent.

In imperative programming languages, it is straightforward to write code which performs "side effects", like:

- throwing an error
- reading from a file
- reading from an environment variable
- writing to a log
- mutating a local variable
- non-determinism: multiple computation paths are taken

A standard way in Haskell to have these abilities in a pure functional setting is with the use of types like `Maybe`, `State` or `Reader`. Because these have `Monad` [instances](/typeclasses/survey/#monad), one can write imperative style code like:

```hs
example :: State Bool ()
example = do
    flag <- get
    put (not flag) 
```


## Transformers

Simple monads tend to only express one of the above effects, so a system for combining them is needed. One approach is to build more complex types modularly, like `StateT Bool (Except Text)` and define the corresponding `Monad` instances in a library.

This is what the [transformers](https://hackage.haskell.org/package/transformers) library does, thus called because `StateT`, `ExceptT`, `ReaderT` and so on are known as *monad transformers*. Here are some of their definitions:

- `#!hs data StateT s m a = StateT {runStateT :: s -> m (a, s)}`
- `#!hs newtype ExceptT e m a = ExceptT {runExceptT :: m (Either e a)}`

??? Note
    `StateT` (like `ExceptT`) is parametrized by three types:
    
    - the type of the state `s`
    - the type of the underlying monad `m`
    - the return type of the whole monad, `a`.

    It has kind: `#!hs * -> (* -> *) -> * -> *`.

For example, `StateT Bool (Except Text) Int` is equivalent to:

```hs 
(Bool -> Either Text (Int, Bool))
```

so represents an `Int` value or failure that results from a boolean state (which could change).

??? Note

    Meanwhile, `ExceptT Text (State Bool) Int` is equivalent to:

    ```hs 
    (Bool -> (Either Text Int, Bool))
    ```

    which is similar, except that the new state is obtained whether or not there is a failure.




However, these types (sometimes referred to as *monad transformer stacks*) tend to be cumbersome to work with, and involve the ungainly use of a `lift` function to coerce types into the right shapes.

## Abstracting the typeclasses

`mtl`[^1] is a widely used library designed to make the experience smoother, which supplies typeclasses that work in conjunction with `transformers`. Here is an example with error throwing and local variable updating:

=== "with `if`"

    ```haskell
   example :: (MonadError Text m, MonadState Bool m) => m Bool
    example = do
        flag <- get
        if flag
            then 
                put (not flag) 
                >> throwError "flag should not have been on!"
            else pure ()
        return flag
    ```

=== "with `when`"

    ```haskell
   example :: (MonadError Text m, MonadState Bool m) => m Bool
    example = do
        flag <- get
        when flag $ 
            put (not flag) 
            >> throwError "flag should not have been on!"
        return flag
    ```

The type of `example` is abstracted over the concrete monad transformer stack, which could either be `ExceptT Text (State Bool)` or `StateT Bool (Except Text)`. 

Depending on how `example` is called, either of these can end up being the concrete type that is inferred and used:

=== "Error over state"

    ```haskell
   errorOverState :: Bool -> IO ()
    errorOverState flagVal = 
        let (result, state) = flip runState flagVal $ runExceptT example
        in do
            putStrLn ("Result: " <> show result)
            putStrLn ("State: " <> show state)
    ```

    results in:

    ```hs title="repl example"
    > errorOverState False
    Result: Right False
    State: False
    > errorOverState True
    Result: Left "flag should not have been on!"
    State: False
    ```
    

=== "State over error"

    ```hs 
    stateOverError :: Bool -> IO ()
    stateOverError flagVal = case runExcept $ flip runStateT flagVal example of
        Left err -> putStrLn ("Error: " <> show err)
        Right (result, state) -> do
            putStrLn ("Result: " <> show result)
            putStrLn ("State: " <> show state) 
    ```

    results in

    ```hs title="repl example"
    > stateOverError False
    Result: False
    State: False
    > stateOverError True
    Error: "flag should not have been on!"
    State: False
    ```


??? Note
    `Except` is a type synonym for `ExceptT Identity`, where `Identity` is the monad which does nothing whatsoever.


[^1]: Short for: monad transformer library

!!! Gotcha

    Alternatives to `mtl` have become popular in recent years, such as [Polysemy](https://github.com/polysemy-research/polysemy#readme) and [cleff](https://github.com/re-xyr/cleff#readme). These are often experimental, or require slightly more advanced use of types (e.g. [type level lists](https://www.parsonsmatt.org/2017/04/26/basic_type_level_programming_in_haskell.html)) and so are recommended for experienced Haskellers only.