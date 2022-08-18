module Chapter1 where

outputLength :: Int
outputLength = 10

inputList = [True, True, False, False]


result :: [a] -> [a]
result =
    fmap snd 
    . take outputLength 
    . filter (not . even . fst) 
    . zip [1..] 