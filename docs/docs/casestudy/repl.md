
```hs title="REPL.hs" linenums="1"
--8<-- "../src/REPL.hs"
```

1. Allows writing e.g. `functionOf do ...` instead of `functionOf $ do`.
2. `void` is a convenient function, which turns `IO a` to `IO ()`. Used here because we don't care about the output of the repl, and in fact that output is never reached, because the repl runs a loop indefinitely.
3. `forever` takes a value of type `m X` (where `m` has a `Monad` constraint on it) and loops it: `forever mX = mX >> forever mX`. It's a clean way to write a for-loop.
4. Get a line from the user.
5. Parse the user's input, using `parse` from the `Parser` module.
6. Get the current state of the board.
7. `hiding` is convenient for avoiding namespace conflicts.
8. `pure` is like `return`, but only requires the `Applicative` typeclass. It can be used almost everywhere that `return` is used, but is strictly more general, because a `Monad` constraint implies an `Applicative` constraint.
9. `catchError`, here used in infix form, stops an error percolating to the top level. This is useful here, because an uncaught error would halt `main`, and so exit the repl.
10. Neither `return` nor `pure` is a keyword, and a block of `do-notation` does not need to end with it. All that is needed at the end of the "do-block" is a value of type `m a` (for the monad `m` in question), and `displayLine result` has that type.
11. `catchError` chooses what to do with the error it catches, and one option is to throw it again. It does this for errors including `Exit`, in order to exit the repl on ":q".
12. `runReplWithBoard` is the function responsible for "unpacking" the monadic value into something simpler. This is sometimes referred to as "running the side effects" of a program.
13. `flip` is a useful function that takes a function of type `X -> Y -> Z` and flips the argument order to give a function of type `Y -> X -> Z`. It is often useful when writing in [pointfree style](/thinkingfunctionally/hof/#pointfree-code).
14. It is often useful to lift the type `IO X` to the more abstract `#!hs MonadIO m => m X`, which is what `liftIO` does.

## Analysis

This module is responsible for producing the actual runnable program (of type `IO ()`) that wraps up the whole system.

## `main`

=== "original"

    ```hs
    main :: IO ()
    main =
    void $ -- (2)!
        runReplWithBoard $
        displayLine "Welcome!\n\n" >> forever do -- (3)!

            line <- requestLine "> " -- (4)!
            let instruction = parse line -- (5)!
            board <- get -- (6)!
            result <-
            case instruction of
                Left (ParseError err) -> pure err -- (8)!
                Left (ReplError err) -> pure err
                Right instr -> evaluate instr
                `catchError` ( \case -- (9)!
                                ReplError txt -> pure txt
                                err -> throwError err -- (11)!
                            )

            displayLine result -- (10)!
    ```

=== "without `forever`"

    ```hs
    main :: IO ()
    main =
        runReplWithBoard $
        displayLine "Welcome!\n\n" >> loop where 
            
            loop = do -- (3)!

            line <- requestLine "> " -- (4)!
            let instruction = parse line -- (5)!
            board <- get -- (6)!
            result <-
                case instruction of
                Left (ParseError err) -> pure err -- (8)!
                Left (ReplError err) -> pure err
                Right instr -> evaluate instr
                    `catchError` ( \case -- (9)!
                                    ReplError txt -> pure txt
                                    err -> throwError err -- (11)!
                                )

            displayLine result -- (10)!
            loop
    ```

=== "without `void`"

    ```hs
    main :: IO ()
    main = do
        runReplWithBoard $
            displayLine "Welcome!\n\n" >> forever do -- (3)!

                line <- requestLine "> " -- (4)!
                let instruction = parse line -- (5)!
                board <- get -- (6)!
                result <-
                case instruction of
                    Left (ParseError err) -> pure err -- (8)!
                    Left (ReplError err) -> pure err
                    Right instr -> evaluate instr
                    `catchError` ( \case -- (9)!
                                    ReplError txt -> pure txt
                                    err -> throwError err -- (11)!
                                )

                displayLine result -- (10)!
        pure ()
    ```