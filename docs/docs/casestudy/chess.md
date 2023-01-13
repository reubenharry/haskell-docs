# Under :construction:


```hs title="Chess.hs" linenums="1"
--8<-- "../src/Chess.hs"
```

1. A [sum](/basics/createdata/#sums) type.
2. 
3. 
4. 

## Comments

## Alternative approaches

=== "original"

    ```hs
    mkRank :: Int -> Maybe Rank 
    mkRank i  
        | inRange i = Just $ R i 
        | otherwise = Nothing

        where 

            inRange n = n `elem` [1..8]
    ```

=== "with brackets"

    ```hs
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

    ```hs
    mkRank :: Int -> Maybe Rank 
    mkRank i  
        | inRange i = Just $ R i 
        | otherwise = Nothing

        where 

            inRange = (`elem` [1..8])
    ```

