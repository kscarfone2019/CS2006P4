-- |Input. Any user Input is handles is here.
module Input(handleInput) where

import Graphics.Gloss.Interface.Pure.Game
import Graphics.Gloss
import Board
import AI
import Draw

import Debug.Trace
import Data.Maybe
import Data.List.Split




-- |Update the world state given an input event.
handleInput :: Event -- ^The event the player has made.
											-> World -- ^The current world state.
											-> IO World -- ^New updated IO World.
handleInput (EventKey (MouseButton RightButton) Up m (x, y)) world | length (pieces (board world)) > 0 && (won world) == False = do return (undoMove world)
                                                                   | otherwise = do return (world)

handleInput (EventKey (MouseButton LeftButton) Up m (x, y)) world |(won world) == False && ((time world == False) || ((time world == True) && (pause world == False))) && (makeMove (board world) (turn world) ((convertToColumnMultiColumn x (board world)),(convertToColumnMultiRow y (board world)))) /= Nothing = do return (World (fromJust(makeMove (board world) (turn world) ((convertToColumnMultiColumn x (board world)),(convertToColumnMultiRow y (board world))))) (other (turn world)) (won world) (winner world) False (pause world) 10 (time world) (pics world) (squares world) (counterOne world) (counterTwo world))
                                                                  |(won world) == False && x<(-140) && x>(-260) && y<(-330) && y>(-410) && length (pieces (board world)) > 0 = do return (undoMove world)
                                                                  |x<(-15) && x>(-175) && y<(-330) && y>(-410) = do return (restartGame world)
                                                                  |(won world) == False && x<(-340) && x>(-460) && y<300 && y>240 = do return (increaseBoardSize world)
                                                                  |(won world) == False && x<(-340) && x>(-460) && y<190 && y>130 = do return (decreaseBoardSize world)
                                                                  |(won world) == False &&  x<(-340) && x>(-460) && y>80 && y<140 = do return (increaseLineSize world)
                                                                  |(won world) == False && x<(-340) && x>(-460) && y>(-40) && y<20 = do return (decreaseLineSize world)
																																	|(won world) == False && x<(468) && x>(338) && y>(-45) && y<75 = do return (changeImage world)
																																  |(won world) == False && x<(-340) && x>(-460) && y>(-125) && y<(-25) = do
																																																																				writeFile "saveFile.txt" (show(world))
																																																																				return world
																																	|(won world) == False && x<(-340) && x>(-460) && y>(-200) && y<(-100) = do
																																																																					contents <- readFile "saveFile.txt"
																																																																					arg <- return(splitOn "\n" contents)
																																																																					return (World (read(arg!!0)) (read(arg!!1)) (read(arg!!2)) (read(arg!!3)) (read(arg!!4)) (read(arg!!5)) (read(arg!!6)) (read(arg!!7)) (read(arg!!8)) (squares world) (counterOne world) (counterTwo world))
																																	|(won world) == False && x<(460) && x>(340) && y>(160) && y<(270) = do return (World (board world) (turn world) (won world) (winner world) True (pause world) (timer world) (time world) (pics world) (squares world) (counterOne world) (counterTwo world))
																																	|(won world) == False && (time world) == False && x<(468) && x>(338) && y>(66) && y<(134) = do return (World (board world) (turn world) (won world) (winner world) (hint world) (pause world) 10 True (pics world) (squares world) (counterOne world) (counterTwo world))
																																	|(won world) == False && (time world) == True && x<(468) && x>(338) && y>(66) && y<(134) = do return (World (board world) (turn world) (won world) (winner world) (hint world) (pause world) 10 False (pics world) (squares world) (counterOne world) (counterTwo world))
																																	|(won world) == False && (pause world) == False && x<(245) && x>(105) && y>(-410) && y<(-330) = do return (World (board world) (turn world) (won world) (winner world) (hint world) True (timer world) (time world) (pics world) (squares world) (counterOne world) (counterTwo world))
																																	|(won world) == False && (pause world) == True && x<(245) && x>(105) && y>(-410) && y<(-330) = do return (World (board world) (turn world) (won world) (winner world) (hint world) False (timer world) (time world) (pics world) (squares world) (counterOne world) (counterTwo world))
																																	|otherwise = do return (world)
handleInput e world =  do return (world)
