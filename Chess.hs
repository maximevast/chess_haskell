-- Chess.hs
module Chess where

import Board
import Utils

import Data.Maybe
import Data.List

linear, diagonal, shape_L, vertical::[Position]
linear   = [(0,1),(0,-1),(1,0),(-1,0)]
diagonal = [(1,1),(-1,-1),(1,-1),(-1,1)]
shape_L  = [(1,2),(2,1),(-1,2),(2,-1),(-2,1),(1,-2),(-1,-2),(-2,-1)]
vertical = [(0,1), (0,-1)]

legal::PieceType->[Position]
legal Rook    = linear
legal Bishop  = diagonal
legal Knight  = shape_L
legal King    = linear ++ diagonal
legal Queen   = linear ++ diagonal
legal Pawn    = vertical ++ diagonal

-- Maximum depth of a movement
depth::PieceType->Int
depth Rook    = 7
depth Bishop  = 7
depth Knight  = 1
depth King    = 2 -- normal = 1; castling = 2
depth Queen   = 7
depth Pawn    = 2 -- normal = 1; from initial pos = 2

deleteAt::Pieces->Int->Pieces
deleteAt ps index = newBoard
  where
    (x,_:ys) = splitAt index ps
    newBoard = x++ys

updateBoard::Pieces->Int->Piece->Pieces
updateBoard ps index newPiece = newBoard
  where
    (x,_:ys) = splitAt index ps
    newBoard = x ++ newPiece : ys

movePiece::Piece->Position->Piece
movePiece piece new_pos = piece { _position = new_pos }

pathEmpty::Pieces->Position->Position->Bool
pathEmpty ps pos1 pos2 = and (map (==Nothing) (map (indexOf ps) (casesBetween pos1 pos2)))

isValidMove::Pieces->Position->Position->Bool
isValidMove ps pos1 pos2
  | pos1 < (1,1) || pos1 > (8,8)  = False -- is initial pos out of bound ?
  | pos2 < (1,1) || pos2 > (8,8)  = False -- is targeted pos out of bound ?
  | pos1 == pos2                  = False -- are we staying at the same pos ?
  | i1 == Nothing                 = False -- is there something to move ?
  | i2 /= Nothing && c1 == c2     = False -- is there an ally on targeted pos ?
  | not (elem pos2 legalMoves)    = False -- is targeted pos in the legal list (depending on piece type) ?
  | otherwise                     = case t1 of
                                      King
                                        | not castling && dist /= 1 -> False
                                        | castling && fst pos2 == 7 &&
                                          (not (pathEmpty ps pos1 ((fst pos2 +1), snd pos2)) ||
                                          rri == Nothing || _type (rr) /= Rook) -> False          --castling right
                                        | castling && fst pos2 == 3 &&
                                          (not (pathEmpty ps pos1 ((fst pos2 -2), snd pos2)) ||
                                          rri == Nothing || _type (rr) /= Rook) -> False          --castling left
                                        | otherwise -> True
                                        where
                                          castling = (fst pos1 == 5 && (snd pos1 == 1 || snd pos1 == 8) && (snd pos1 == snd pos2) && (fst pos2 == 7 || fst pos2 == 3))
                                          lri = indexOf ps (1, snd pos1)  -- maybe left rook index
                                          rri = indexOf ps (8, snd pos1)  -- maybe right rook index
                                          lr = getPiece ps (fromJust lri) -- left rook
                                          rr = getPiece ps (fromJust rri) -- right rook
                                      Queen
                                        | not (pathEmpty ps pos1 pos2) -> False
                                        | otherwise -> True
                                      Rook
                                        | not (pathEmpty ps pos1 pos2) -> False
                                        | otherwise -> True
                                      Knight -> True
                                      Bishop
                                        | not (pathEmpty ps pos1 pos2) -> False
                                        | otherwise -> True
                                      Pawn
                                        | c1 == White && snd pos1 /= 2 && dist /= 1       -> False -- basic distance checking
                                        | c1 == Black && snd pos1 /= 7 && dist /= 1       -> False -- basic distance checking
                                        | c1 == White && (snd pos2 - snd pos1) < 1        -> False -- a pawn can't move backward
                                        | c1 == Black && (snd pos1 - snd pos2) < 1        -> False -- a pawn can't move backward
                                        | c1 == White && snd pos1 == 2 && dist > 2        -> False -- distance checking for initial pos
                                        | c1 == Black && snd pos1 == 7 && dist > 2        -> False -- distance checking for initial pos
                                        | dir == Linear && i2 /= Nothing                  -> False -- pawn can only capture diagonaly
                                        | dir == Diagonal && (dist /= 1 || i2 == Nothing) -> False -- diagonal is only for capture (with distance = 1)
                                        | otherwise -> True
  where
    i1 = indexOf ps pos1
    i2 = indexOf ps pos2
    p1 = getPiece ps (fromJust i1)
    p2 = getPiece ps (fromJust i2)
    t1 = _type (p1)
    c1 = _color (p1)
    c2 = _color (p2)
    legalMoves = concatMap (\x -> (take (depth t1) $ tail $ iterate (addPositions x) pos1)) (legal t1) -- compute list of legal pos
    dist = getDistance pos1 pos2
    dir  = getDirection pos1 pos2
