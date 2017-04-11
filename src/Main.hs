module Main where

import Graphics.Gloss

import System.Environment

import Board
import Draw
import Input
import AI

-- 'play' starts up a graphics window and sets up handlers for dealing
-- with inputs and updating the world state.
--
-- 'drawWorld' converts the world state into a gloss Picture
--
-- 'handleInput' is called whenever there is an input event, and if it is
-- a human player's turn should update the board with the move indicated by
-- the event
--
-- 'updateWorld' is called 10 times per second (that's the "10" parameter)
-- and, if it is an AI's turn, should update the board with an AI generated
-- move

main :: IO ()
main = do
  args <- getArgs
  let initBoard = if length args > 0 && ((read (head (tail args)) ::Int )== 3 || (read (head (tail args)) ::Int) == 5) && ((read (head args) ::Int)>5 && (read (head args) ::Int)<20)
                  then Board (read (head args) ::Int) (read (head (tail args)) ::Int) []
                  else Board 6 3 []
  let initWorld = World initBoard Black False Empty savedBoard
  play (InWindow "Gomoku" (900, 900) (10, 10)) black 10
            initWorld -- in Board.hs
            drawWorld -- in Draw.hs
            handleInput -- in Input.hs
            updateWorld -- in AI.hs
