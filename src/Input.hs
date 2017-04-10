module Input(handleInput) where

import Graphics.Gloss.Interface.Pure.Game
import Graphics.Gloss
import Board
import AI
import Draw

import Debug.Trace
import Data.Maybe

-- Update the world state given an input event.
handleInput :: Event -> World -> World
--handleInput (EventMotion (x, y)) world = trace ("Mouse moved to: " ++ show (x,y)) world
handleInput (EventKey (MouseButton RightButton) Up m (x, y)) world | length (pieces (board world)) > 0 && (won world) == False = undoMove world
                                                                   | otherwise = world

handleInput (EventKey (MouseButton LeftButton) Up m (x, y)) world |(won world) == False && (makeMove (board world) (turn world) ((convertToColumnMultiColumn x (board world)),(convertToColumnMultiRow y (board world)))) /= Nothing = (World (fromJust(makeMove (board world) (turn world) ((convertToColumnMultiColumn x (board world)),(convertToColumnMultiRow y (board world))))) (other (turn world)) (won world) (winner world))
                                                                  |(won world) == False && x<(-140) && x>(-260) && y<(-310) && y>(-390) && length (pieces (board world)) > 0 = undoMove world
                                                                  |x<(-15) && x>(-175) && y<(-320) && y>(-400) = restartGame world
                                                                  |otherwise = world

--handleInput (EventKey (Char k) Down _ _) world = trace ("Key " ++ show k ++ " down") world
--handleInput (EventKey (Char k) Up _ _) world = trace ("Key " ++ show k ++ " up") world
handleInput e world = world
