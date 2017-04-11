module Input(handleInput) where

import Graphics.Gloss.Interface.Pure.Game
import Graphics.Gloss
import Board
import AI
import Draw

import Debug.Trace
import Data.Maybe




printSaveButton :: Picture
printSaveButton = Pictures [
													Color violet (Translate (-400) (-75) (rectangleSolid 60 50)),
													Color white (Translate (-422) (-72) (Scale 0.15 0.15 (Text "Save"))),
													Color white (Translate (-427) (-93) (Scale 0.15 0.15 (Text "Game")))
													]

printLoadButton :: Picture
printLoadButton = Pictures [
													Color violet (Translate (-400) (-150) (rectangleSolid 60 50)),
													Color white (Translate (-422) (-148) (Scale 0.15 0.15 (Text "Load"))),
													Color white (Translate (-427) (-168) (Scale 0.15 0.15 (Text "Game")))
													]

-- Update the world state given an input event.
handleInput :: Event -> World -> World
--handleInput (EventMotion (x, y)) world = trace ("Mouse moved to: " ++ show (x,y)) world
handleInput (EventKey (MouseButton RightButton) Up m (x, y)) world | length (pieces (board world)) > 0 && (won world) == False = undoMove world
                                                                   | otherwise = world

handleInput (EventKey (MouseButton LeftButton) Up m (x, y)) world |(won world) == False && (makeMove (board world) (turn world) ((convertToColumnMultiColumn x (board world)),(convertToColumnMultiRow y (board world)))) /= Nothing = (World (fromJust(makeMove (board world) (turn world) ((convertToColumnMultiColumn x (board world)),(convertToColumnMultiRow y (board world))))) (other (turn world)) (won world) (winner world) (saveBoard world))
                                                                  |(won world) == False && x<(-140) && x>(-260) && y<(-330) && y>(-410) && length (pieces (board world)) > 0 = undoMove world
                                                                  |x<(-15) && x>(-175) && y<(-330) && y>(-410) = restartGame world
                                                                  |(won world) == False && x<(-340) && x>(-460) && y<300 && y>240 = increaseBoardSize world
                                                                  |(won world) == False && x<(-340) && x>(-460) && y<190 && y>130 = decreaseBoardSize world
                                                                  |(won world) == False &&  x<(-340) && x>(-460) && y>80 && y<140 = increaseLineSize world
                                                                  |(won world) == False && x<(-340) && x>(-460) && y>(-40) && y<20 = decreaseLineSize world
																																  |(won world) == False && x<(-340) && x>(-460) && y>(-125) && y<(-25) = saveGame world
																																	|(won world) == False && x<(-340) && x>(-460) && y>(-200) && y<(-100) = loadGame world
                                                                  |otherwise = world

--handleInput (EventKey (Char k) Down _ _) world = trace ("Key " ++ show k ++ " down") world
--handleInput (EventKey (Char k) Up _ _) world = trace ("Key " ++ show k ++ " up") world
handleInput e world = world
