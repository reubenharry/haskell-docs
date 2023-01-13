--
-- UNDER CONSTRUCTION!!
--

module Chess where


data PieceType = Bishop | Rook | Knight | Pawn | King | Queen -- (1)!
data Color = Black | White
data Piece = Piece PieceType Color

data Square = Sq Int Int

mkSquare :: Int -> Int -> Maybe Square -- (2)!
mkSquare i j 
    | inRange i && inRange j = Just $ Sq i j -- (3)!
    | otherwise = Nothing

    where -- (4)!

        inRange n = n `elem` [1..8]

data Rank 
data File
data SquareState