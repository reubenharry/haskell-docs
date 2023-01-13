# Under :construction:

This section is a walkthrough of a [project](https://github.com/reubenharry/haskell-docs) with the [code](https://github.com/reubenharry/haskell-docs/tree/main/src) intended to display how to write a simple but real-world application in Haskell.

!!! Tip
    If you encounter an expression in this codebase that you don't understand, use Haskell's purity and immutability to your advantage: you can't always break it down into its parts and understand them separately.

    ```hs title="complicated looking example"
    main :: IO ()
    main = runInputT defaultSettings $ flip evalStateT initBoard $ forever $ do
        line <- lift requestLine 
        ...
    ```

    Break this down by replacing everything after the first or second [dollar sign](/basics/syntax/#dollar-sign) with `undefined`, and then mouse over `undefined` to understand its type:

    ```hs title="simpler"
    main :: IO ()
    main = runInputT defaultSettings undefined
    ```

    Or 

    ```hs title="simpler"
    main :: IO ()
    main = runInputT defaultSettings $ flip evalStateT initBoard $ undefined
    ```

    Or remove everything before the first or second dollar sign and have Haskell infer the type:

    ```hs title="simpler"
    -- main :: IO () -- commented out since the type might be different now
    main = flip evalStateT initBoard $ forever $ do
        line <- lift requestLine 
        ...
    ```

    Consider this the Haskell analogue of inserting print statements.


 for displaying and updating a chess board via the command line:

```
update board
which square?
knight
that is not a square. You need to specify a square
a4
options:
    add piece / remove piece
which piece
black knight
which color 
black
```

The goal of this walkthrough is to show how to write a simple, but non-trivial application in Haskell, using 

- how to read files
- how to handle exceptions
- how to write an interactive program
- how to parse text
- how to implement a domain specific language


    exception handling, common packages (`megaparsec`, `aeson` and `haskeline`)