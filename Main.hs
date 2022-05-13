-- Main.hs
module Main where

import Data.Maybe(fromJust)
import System.Console.ANSI
import Control.Concurrent
import Control.Monad

import Chess
import Board
import Utils

type Moves = [String]

castling::Pieces->Position->Position->Pieces
castling ps pos1 pos2
  | fst pos2 == 7 = updateBoard ps r_rook_index r_rook -- castling right
  | otherwise     = updateBoard ps l_rook_index l_rook -- castling left
  where
    l_rook_index = fromJust(indexOf ps (1, y))
    r_rook_index = fromJust(indexOf ps (8, y))
    l_rook = (movePiece (getPiece ps l_rook_index) (4,y))
    r_rook = (movePiece (getPiece ps r_rook_index) (6,y))
    y = snd pos1

move::Pieces->String->Pieces
move ps order
  | not (isValidMove ps pos1 pos2)      = ps
  | inde_t == Nothing && not isCastling = updateBoard ps inde_c piec
  | inde_t == Nothing && isCastling     = updateBoard (castling ps pos1 pos2) inde_c piec
  | otherwise = deleteAt (updateBoard ps inde_c piec) (fromJust inde_t)
  where
    piec = (movePiece (getPiece ps inde_c) pos2)
    inde_c = fromJust(indexOf ps pos1)
    inde_t = indexOf ps pos2
    pos1 = chessToPosition(take 2 order)
    pos2 = chessToPosition(drop 2 order)
    typ = _type (getPiece ps inde_c)
    isCastling = (typ == King) && (fst pos1 == 5 && (snd pos1 == 1 || snd pos1 == 8) &&
                 (snd pos1 == snd pos2) && (fst pos2 == 7 || fst pos2 == 3))

getInput::IO String
getInput = do
  input <- getLine
  if (length input == 4)
    then return input
    else getInput

game::Pieces->IO()
game ps = do
  clearScreen
  putStr $ printBoard ps
  input <- getInput
  game (move ps input)

gameLoop::Pieces->Moves->IO()
gameLoop ps ms = do
  clearScreen
  putStr $ printBoard ps
  threadDelay (700000) --0.7 seconds
  unless (null ms) (gameLoop (move ps (ms!!0)) (tail ms))

main::IO()
main = do
  game pieceList

scholarsMate, withCastling ::Moves
scholarsMate = ["e2e4","e7e5","d1h5","b8c6","f1c4","g8f6","h5f7"]

withCastling = ["d2d4","g8f6","c2c4","e7e6","b1c3","f8b4","e2e3","e8g8","f1d3","d7d5","g1f3",
      "b7b6","a2a3","b4e7","c4d5","e6d5","b2b4","c7c5","d4c5","b6c5","b4c5","e7c5",
      "e1g1","d8e7","c1b2","b8c6","d1a4","c8b7","f1c1","c5d6","a4h4","c6e5","f3e5"]
