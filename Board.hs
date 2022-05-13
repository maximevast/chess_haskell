-- Board.hs
module Board where

import Data.List
import Data.Maybe

data PieceColor = Black | White deriving Eq
data PieceType = Rook | Knight | Bishop | King | Queen | Pawn deriving Eq
type Position = (Int, Int)
data Piece = Piece {
  _type :: PieceType,
  _color :: PieceColor,
  _position :: Position
  } deriving (Eq)
type Pieces = [Piece]
type Board = [[Position]]

instance Show Piece where
 show (Piece King White _)    = "K"
 show (Piece Queen White _)   = "Q"
 show (Piece Rook White _)    = "R"
 show (Piece Bishop White _)  = "B"
 show (Piece Knight White _)  = "N"
 show (Piece Pawn White _)    = "P"
 show (Piece King Black _)    = "k"
 show (Piece Queen Black _)   = "q"
 show (Piece Rook Black _)    = "r"
 show (Piece Bishop Black _)  = "b"
 show (Piece Knight Black _)  = "n"
 show (Piece Pawn Black _)    = "p"


indexOf::Pieces->(Int,Int)->Maybe Int
indexOf ps (x,y) = findIndex piece ps
  where
    piece (Piece _ _ pos) = pos == (x,y)

getPiece::Pieces->Int->Piece
getPiece ps index = ps!!index

printSquare::Pieces->Position->String
printSquare ps p
  | index == Nothing = "-- "
  | otherwise = show ((getPiece ps (fromJust index))) ++ "  "
  where
    index = indexOf ps p

printBoard::Pieces->String
printBoard ps = unlines (map (concatMap (printSquare ps )) gameboard)

gameboard::Board
gameboard = [
          [(1,8), (2,8), (3,8), (4,8), (5,8), (6,8), (7,8), (8,8)],
          [(1,7), (2,7), (3,7), (4,7), (5,7), (6,7), (7,7), (8,7)],
          [(1,6), (2,6), (3,6), (4,6), (5,6), (6,6), (7,6), (8,6)],
          [(1,5), (2,5), (3,5), (4,5), (5,5), (6,5), (7,5), (8,5)],
          [(1,4), (2,4), (3,4), (4,4), (5,4), (6,4), (7,4), (8,4)],
          [(1,3), (2,3), (3,3), (4,3), (5,3), (6,3), (7,3), (8,3)],
          [(1,2), (2,2), (3,2), (4,2), (5,2), (6,2), (7,2), (8,2)],
          [(1,1), (2,1), (3,1), (4,1), (5,1), (6,1), (7,1), (8,1)]
        ]

pieceList::Pieces
pieceList = [ (Piece Rook Black (1,8)), (Piece Knight Black (2,8)), (Piece Bishop Black (3,8)), (Piece Queen Black (4,8)),
          (Piece King Black (5,8)), (Piece Bishop Black (6,8)), (Piece Knight Black (7,8)), (Piece Rook Black (8,8)),
          (Piece Pawn Black (1,7)), (Piece Pawn Black (2,7)), (Piece Pawn Black (3,7)), (Piece Pawn Black (4,7)),
          (Piece Pawn Black (5,7)), (Piece Pawn Black (6,7)), (Piece Pawn Black (7,7)), (Piece Pawn Black (8,7)),
          (Piece Pawn White (1,2)), (Piece Pawn White (2,2)), (Piece Pawn White (3,2)), (Piece Pawn White (4,2)),
          (Piece Pawn White (5,2)), (Piece Pawn White (6,2)), (Piece Pawn White (7,2)), (Piece Pawn White (8,2)),
          (Piece Rook White (1,1)), (Piece Knight White (2,1)), (Piece Bishop White (3,1)), (Piece Queen White (4,1)),
          (Piece King White (5,1)), (Piece Bishop White (6,1)), (Piece Knight White (7,1)), (Piece Rook White (8,1))]
