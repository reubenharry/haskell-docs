module Chapter1 where


outputLength :: Int
outputLength = 5

inputList :: [Double]
inputList = [1..20]

takeEveryOther :: [a] -> [a]
takeEveryOther =
    fmap snd 
    . take outputLength 
    . filter (not . even . fst) 
    . zip [1..] 

result :: IO ()
result = print (takeEveryOther inputList)