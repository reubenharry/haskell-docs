---
comments: true
---

This section is a walkthrough of a [project](https://github.com/reubenharry/haskell-docs) with the [code](https://github.com/reubenharry/haskell-docs/tree/main/src) intended to display how to write a simple but real-world application in Haskell.

The subsections are ordered by difficulty.

!!! Tip
    We strongly encourage reading Haskell code in a text editor with the [IDE](/gettingstarted/overview/#step-3-set-up-the-haskell-language-server) activated: mousing over variables will show their type and where they are defined, which is especially helpful when learning. ++command++ + `click` to jump to a definition.

## Demo:

```sh
cabal run chess-repl 

Welcome!


> 
place a white bishop on a5
_|_|_|_|b|_|_|_
_|_|_|_|_|_|_|_
_|_|_|_|_|_|_|_
_|_|_|_|_|_|_|_
_|_|_|_|_|_|_|_
_|_|_|_|_|_|_|_
_|_|_|_|_|_|_|_
_|_|_|_|_|_|_|_
> 
place a black castle on b6
1:15:
  |
1 | place a black castle on b6
  |               ^^^^^^
unexpected "castle"
expecting "bishop", "king", "knight", "pawn", "queen", "rook", or white space

> 
:d
_|_|_|_|b|_|_|_
_|_|_|_|_|_|_|_
_|_|_|_|_|_|_|_
_|_|_|_|_|_|_|_
_|_|_|_|_|_|_|_
_|_|_|_|_|_|_|_
_|_|_|_|_|_|_|_
_|_|_|_|_|_|_|_
> 
:q
```

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

    Consider this the Haskell debugging analogue of inserting print statements.

