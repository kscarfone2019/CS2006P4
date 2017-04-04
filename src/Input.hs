module Input(handleInput) where

import Graphics.Gloss.Interface.Pure.Game
import Graphics.Gloss
import Board
import AI

import Debug.Trace
import Data.Maybe


convertToColumn :: Float -> Int
convertToColumn x |x<(-212.5) && x>(-287.5) = 1
                  |x<(-112.5) && x>(-187.5) = 2
                  |x<(-12.5) && x>(-87.5) = 3
                  |x<87.5 && x>12.5 = 4
                  |x<187.5 && x>112.5 = 5
                  |x<287.5 && x>212.5 = 6
                  |otherwise = 0

convertToRow :: Float -> Int
convertToRow y |y>212.5 && y< 287.5 = 1
               |y>112.5 && y< 187.5 = 2
               |y>12.5 && y< 87.5 = 3
               |y>(-87.5) && y< (-12.5) = 4
               |y>(-187.5) && y< (-112.5) = 5
               |y>(-287.5) && y< (-212.5) = 6
               |otherwise = 0

-- Update the world state given an input event.
handleInput :: Event -> World -> World
--handleInput (EventMotion (x, y)) world = trace ("Mouse moved to: " ++ show (x,y)) world
handleInput (EventKey (MouseButton RightButton) Up m (x, y)) world | length (pieces (board world)) > 0 && (won world) == False = undoMove world
                                                                   | otherwise = world

handleInput (EventKey (MouseButton LeftButton) Up m (x, y)) world |(won world) == False && (makeMove (board world) (turn world) ((convertToColumn x),(convertToRow y))) /= Nothing = (World (fromJust(makeMove (board world) (turn world) ((convertToColumn x),(convertToRow y)))) (other (turn world)) (won world) (winner world))
                                                                  |(won world) == False && x<(-140) && x>(-260) && y<(-310) && y>(-390) && length (pieces (board world)) > 0 = undoMove world
                                                                  |x<(-15) && x>(-175) && y<(-310) && y>(-390) = restartGame world
                                                                  |otherwise = world

--handleInput (EventKey (Char k) Down _ _) world = trace ("Key " ++ show k ++ " down") world
--handleInput (EventKey (Char k) Up _ _) world = trace ("Key " ++ show k ++ " up") world
handleInput e world = world
