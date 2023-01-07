{-# LANGUAGE LambdaCase #-}
module MyLib (someFunc) where
import Control.Monad (forM)

someFunc :: IO ()
someFunc = putStrLn "someFunc"

expr = Just (1,2)

boolToNum bool = case bool of 
    True -> 1
    False -> 0


data ChessPiece = Piece PieceType Color
data Color = Black | White
data PieceType = Bishop | Rook

isPieceWhite = \case
    Piece _ White -> True
    _ -> False

-- example :: 
-- example :: IO [Integer]
example :: Int -> Bool
example x 
    | x > 0 && x < 10 = True
    | otherwise = False