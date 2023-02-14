---
comments: true
---


```hs title="Chess.hs" linenums="1"
--8<-- "../src/Chess.hs"
```

1. A [sum](/basics/createdata/#sums) type.
2. A [product](/basics/createdata/#products) type.
3. Module should have same name as file.
4. Automatically [derive](/typeclasses/overview/#automatically-deriving-instances) [Eq](/typeclasses/survey/#eq) and [Show](/typeclasses/survey/#show) [typeclasses](/typeclasses/overview)
5. `Enum` allows for writing e.g. `[A .. H]`
6. Alternative approaches [include](/casestudy/chess/#initboard)
7. Another syntax for custom datatypes, know as [GADT](/basics/createdata/#products)
8. A list comprehension, as in Python.
9. `into` is from the [witch](/faqs/convertingnumbers) package, for type coercions. `@Text` indicates the type to coerce to.
10. No need to write type signatures (although it's good practice) - Haskell will infer them for you.
11. `\case` requires the `LambdaCase` extension which has been globally enabled in the project's `.cabal` file.
12. If `Black`, return function to uppercase the character.
13. If `White`, don't change the letter case. `id` can be useful.
14. `newtype` is like the `data` keyword. See more about `newtype` [here](https://kowainik.github.io/posts/haskell-mini-patterns)
15. Alternative approaches to the `Board` type [include](/casestudy/chess/#board).

## Analysis

Because custom types are so easily made and expressive, it is typical in Haskell to create types that model your problem domain (here various chess-related types).

The central type is `Board`, which represents the state of a chessboard. We have chosen to directly represent the board's state as a function from a square (specified by file and rank) to the square's state. 

A good example of [type-based refactoring](/thinkingfunctionally/typeinference/#type-based-refactoring) in Haskell is to change `Board` to an [alternative representation](/casestudy/chess/#board) and then fix the series of type errors that Haskell will show you in order to make the new `Board` type work across your project.

## Alternative approaches

### `initBoard`

=== "original"

    ```haskell
    initBoard :: Board
    initBoard = Board $ \f r -> Empty
    ```

=== "wildcards"

    ```haskell
    initBoard :: Board
    initBoard = Board $ \_ _ -> Empty
    ```

=== "with `curry` and `const`"

    ```haskell
    initBoard :: Board
    initBoard = Board $ curry $ const Empty
    ```

    !!! Tip 
        To understand how this works, lookup the types of `const` and `curry` on [Hoogle](/gettingstarted/overview/#step-4-hoogle) (both are common and useful functions).
        
        Then ascertain the type of `const Empty` with VSCode, namely:

        - `#!hs (const Empty) :: (File, Rank) -> SquareState`

        Convince yourself that this is an appropriate input type for `curry`, and an appropriate output type for `const`.

### `Board`

=== "original"

    ```hs
    data SquareState where 
        Empty :: SquareState
        HasPiece :: Piece -> SquareState

    newtype Board where
        Board :: (File -> Rank -> SquareState) -> Board
    ```

=== "with dictionary"

    ```hs
    import qualified Data.Map as M
    type Board = M.Map (File, Rank) Piece
    ```


<!-- ## Alternative approaches

=== "original"

    ```haskell
   mkRank :: Int -> Maybe Rank 
    mkRank i  
        | inRange i = Just $ R i 
        | otherwise = Nothing

        where 

            inRange n = n `elem` [1..8]
    ```

=== "with brackets"

    ```haskell
   mkRank :: Int -> Maybe Rank 
    mkRank i  
        | inRange i = Just (R i)
        | otherwise = Nothing

        where 

            inRange n = n `elem` [1..8]
    ```

=== "with let and if"

    ```hs 
    mkRank :: Int -> Maybe Rank
    mkRank i = 
        let inRange n = n `elem` [1..8]
        in
            if inRange i 
            then Just $ R i 
            else Nothing
    ```



=== "with section"

    ```haskell
   mkRank :: Int -> Maybe Rank 
    mkRank i  
        | inRange i = Just $ R i 
        | otherwise = Nothing

        where 

            inRange = (`elem` [1..8])
    ``` -->

