--
-- UNDER CONSTRUCTION!!
--

module Chess where


data PieceType = Bishop | Rook | Knight | Pawn | King | Queen -- (1)!
data Color = Black | White
data Piece = Piece PieceType Color

data Square = Sq Int Int

data Rank 
data File
data SquareState