--
-- UNDER CONSTRUCTION!!
--

{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
-- {-# LANGUAGE DeriveGeneric #-}
-- {-# LANGUAGE InstanceSigs #-}
-- {-# LANGUAGE TypeApplications #-}

module MyLib (someFunc) where
import Control.Monad (forM)
import Data.Text (Text)
import Data.Coerce (coerce)
import GHC.Generics (Generic)
import Witch
import Control.Monad.State (execState)
import Control.Monad.RWS (modify)

data ChessSquare = Square Int Int
-- data Color = Black | White

squareColor :: ChessSquare -> Color
squareColor (Square i j) 
    | even (i+j) = White
    | otherwise = Black











func = fmap not (\x -> x > 3) 




correct :: Color
correct =  squareColor (Square 2 4)



class Length f where
    len :: f a -> Int

float :: Float
float = 4

double = into @Double float

str :: String
str = "hello"

text :: Text
text = into str

class Functor' (f :: * -> *) where 
    fmal' :: f a



data List a = EmptyList | HeadThenList a (List a)

-- getLeft :: Either a b -> Maybe a
getLeft :: Either Bool b -> Maybe Bool
getLeft (Left x) = Just (not x)
getLeft (Right _) = Nothing

-- instance Length [] where
--     len (x: xs) = length 

-- add1AndPrint :: Bool -> IO Int
-- add1AndPrint x = print x >> return (x + 1)


take' 0 ls = []
take' _ [] = []
take' n (firstElem : rest) = firstElem : take' (n-1) rest

func1 = 1 : func2
func2 = 2 : func1

loop = flip execState 0 $ forM [0..9] $ \i ->
    modify (+i)

class  Eq' a  where
    (===) :: a -> a -> Bool --(1)!

instance Eq' Bool where
    True === True = True
    False === False = True
    _ === _ = False

primes = filterPrime [2..]
  where filterPrime (p:xs) =
          p : filterPrime [x | x <- xs, x `mod` p /= 0]

exFunc whole@(int,bool) 
    | even int = whole
    | otherwise = (int - 1, not bool)

someFunc :: IO ()
someFunc = putStrLn "someFunc"

expr = Just (1,2)

boolToNum bool = case bool of 
    True -> 1
    False -> 0

x = 4
exampleWithDollar = not $ (> 3) x

equals :: Eq a => (->) (a, a) Bool
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
data Color = Black | White deriving Show
data PieceType = Bishop | Rook
-- data Square = Sq Int Int 

data Piece c = Bish c | Knight c | King c

data Piece' = Bishop' | Knight' | Rook' deriving Show

class SemiGrop a where 
    (<||>) :: a -> a -> a

class SemiGrop a => Monoud a where 
    mampty :: a

-- instance SemiGrop Text where
--     t1 <||> t2 = undefined

fooo :: Ord a => ([a], [a]) -> Bool
fooo (x, y) = x == y

exa = foldr best Bishop' [Bishop', Knight', Rook', Bishop'] 
    
    where
    
        best piece1 piece2
            | value piece1 >= value piece2 = piece1
            | otherwise = piece2

        value piece = case piece of
            Bishop' -> 3
            Knight' -> 3
            Rook' -> 5




isPieceWhite = \case
    Piece _ White -> True
    _ -> False

    -- import Data.Text (Text)


pieceToText :: ChessPiece -> Text
pieceToText (Piece _ color) = case color of 
    Black -> "black"
    White -> "white"

foo' = "foo" :: String

bar = into @Text foo'

type Number = Double

shiftByFour maybeFlag x = case maybeFlag of 
    Nothing -> error "define"
    Just flag -> if flag then x + 4 else x - 4

example' input = result <> " tree" 
        where
    result = case input of
        True -> "red"
        False -> "black"

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