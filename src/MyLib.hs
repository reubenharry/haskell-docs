{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE InstanceSigs #-}

module MyLib (someFunc) where
import Control.Monad (forM)
import Data.Text (Text)
import Data.Coerce (coerce)
import GHC.Generics (Generic)

someFunc :: IO ()
someFunc = putStrLn "someFunc"

expr = Just (1,2)

boolToNum bool = case bool of 
    True -> 1
    False -> 0

equals :: Eq a => (a, a) -> Bool
equals (x,y) = x == y 

class Addable a where
    add :: a -> a -> a
    
instance Addable Int where
    add = (+)

-- instance Monoid Entity

data Entity = Sq {row :: Int, col :: Int} | Player Bool -- (1)!

foo = "foo" ::  Text

-- data BinTree = Leaf Int | Branch BinTree BinTree

data BinTree a = Leaf a | Branch (BinTree a) (BinTree a)

data Machine a b = M {runM :: (a -> (b, Machine a b))}

machine :: Machine Int Int
machine = machine1 where 

    machine1 :: Machine Int Int
    machine1 = M (\i -> (i, if i > 10 then machine2 else machine1))

    machine2 :: Machine Int Int
    machine2 = M (\i -> (0, machine2))

fromEntity :: Entity -> Either (Int, Int) Bool
fromEntity (Sq i j) = Left (i, j)
fromEntity (Player bool) = Right bool

toEntity :: Either (Int, Int) Bool -> Entity
toEntity (Left (i ,j)) = Sq i j
toEntity (Right bool) = Player bool


-- fromSq :: Square -> (Int, Int)
-- fromSq (Sq i j) = (i, j)

-- toSq :: (Int, Int) -> Square
-- toSq (i, j) = Sq i j


-- data Square = Sq Int Int 

data ChessPiece = Piece PieceType Color 
data Color = Black | White
data PieceType = Bishop | Rook
-- data Square = Sq Int Int 

data Piece c = Bish c | Knight c | King c



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