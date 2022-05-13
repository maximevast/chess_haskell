module Utils where

import Data.Char (ord)

import Board

data Direction = Linear|Diagonal deriving Eq

chessCharToInteger::Char->Int
chessCharToInteger a = (ord a - 96)

chessIntToInteger::Char->Int
chessIntToInteger a = (ord  a - 48)

chessToPosition::String->Position
chessToPosition [x,y] = (chessCharToInteger x, chessIntToInteger y)

addPositions::Position->Position->Position
addPositions (x1,y1) (x2,y2) = (x1+x2,y1+y2)

createPosition::Int->Int->Position
createPosition a b = (a,b)

invertPostion::Position->Position
invertPostion (a,b) = (b,a)

getDistance::Position->Position->Int
getDistance (x1,y1) (x2,y2)
  | x1 == x2  = abs(y2 - y1)
  | otherwise = abs(x2 - x1)

getDirection::Position->Position->Direction
getDirection (x1,y1) (x2,y2)
  | (x1 == x2) || (y1 == y2) = Linear
  | otherwise = Diagonal

casesBetween::Position->Position->[Position]
casesBetween (x1,y1) (x2,y2)
  | x1 == x2 = map (createPosition x1) yRange                       -- Horizontal case
  | y1 == y2 = map (invertPostion) (map (createPosition y1) xRange) -- vertical case
  | (x1-x2) == (y1-y2) = zipWith createPosition xRange yRange       -- y = x
  | otherwise = zipWith createPosition (reverse xRange) yRange      -- y = -x
  where
    yRange = init(tail [(min y1 y2)..(max y1 y2)])
    xRange = init(tail [(min x1 x2)..(max x1 x2)])
