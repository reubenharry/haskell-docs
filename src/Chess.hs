--
-- UNDER CONSTRUCTION!!
--

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveAnyClass #-}


module Chess where

import qualified Data.Text as T
import Data.Aeson (encode, ToJSON)
import GHC.Generics (Generic)
import Data.ByteString.Lazy (ByteString)
import qualified Data.Map as M

data PieceType = Bishop | Rook | Knight | Pawn | King | Queen -- (1)!
data Color = Black | White
data Piece = Piece PieceType Color  -- (2)!


a :: ByteString
a = encode (M.fromList [('a', 'b')])

data Rank = R Int 
data File = F Int

data SquareState where
  Empty :: SquareState
  HasPiece :: Piece -> SquareState

mkRank :: Int -> Maybe Rank -- (3)!
mkRank i  
    | inRange i = Just $ R i -- (4)!
    | otherwise = Nothing

    where -- (5)!

        inRange n = n `elem` [1..8]


initBoard :: Board 
initBoard = undefined 

newtype Board where
  Lookup :: (Rank -> File -> SquareState) -> Board
  
display :: p -> T.Text
display _ = T.concat $ take 8 $ repeat "| | | | | | | | |\n"

-- use flip, const
