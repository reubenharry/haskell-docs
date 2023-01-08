{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module MyLib (someFunc) where
import Control.Monad (forM)
import Data.Text (Text)

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

    -- import Data.Text (Text)


pieceToText :: ChessPiece -> Text
pieceToText (Piece _ color) = case color of 
    Black -> "black"
    White -> "white"

example = val1 where
    val1 = 0 : val2
    val2 = 1 : val1

-- data ChessPiece = ChessPiece Int Int

squareWithFailure :: Int -> Either Text Int
squareWithFailure i 
    | i >= 0 = Right (i ^ 2)
    | otherwise = Left "squareWithFailure only takes non-negative input"

complicatedFunction :: ChessPiece -> [Int]
complicatedFunction chesspiece = case chesspiece of
    Piece _ Black -> numMovesBlack
    Piece _ White -> numMovesWhite

    where
        numMovesBlack = filter isValidMove allMoves
        numMovesWhite = undefined
        isValidMove = undefined
        allMoves = undefined