module Chapter1 where


outputLength :: Int
outputLength = 5


takeEveryOther :: [a] -> [a]
takeEveryOther =
    fmap snd 
    . take outputLength 
    . filter (not . even . fst) 
    . zip [1..] 

result :: IO ()
result = print (takeEveryOther inputList)