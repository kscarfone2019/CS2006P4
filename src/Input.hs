module Input(handleInput) where

import Graphics.Gloss.Interface.Pure.Game
import Graphics.Gloss
import Board
import AI

import Debug.Trace
import Data.Maybe

-- Update the world state given an input event. Some sample input events
-- are given; when they happen, there is a trace printed on the console
--
-- trace :: String -> a -> a
-- 'trace' returns its second argument while printing its first argument
-- to stderr, which can be a very useful way of debugging!
handleInput :: Event -> World -> World
--handleInput (EventMotion (x, y)) world = trace ("Mouse moved to: " ++ show (x,y)) world
handleInput (EventKey (MouseButton RightButton) Up m (x, y)) world | length (pieces (board world)) > 0 = undoMove world
                                                                   | otherwise = world

handleInput (EventKey (MouseButton LeftButton) Up m (x, y)) world |(won world) == False && x<(-212.5) && x>(-287.5) && y>212.5 && y< 287.5 && (makeMove (board world) (turn world) (1,1)) /= Nothing = (World (fromJust(makeMove (board world) (turn world) (1,1))) (other (turn world)) (won world) (winner world))
																																  |(won world) == False && x<(-212.5) && x>(-287.5) && y>112.5 && y< 187.5 && (makeMove (board world) (turn world) (1,2)) /= Nothing = (World (fromJust(makeMove (board world) (turn world) (1,2))) (other (turn world)) (won world) (winner world))
																																  |(won world) == False && x<(-212.5) && x>(-287.5) && y>12.5 && y< 87.5 && (makeMove (board world) (turn world) (1,3)) /= Nothing = (World (fromJust(makeMove (board world) (turn world) (1,3))) (other (turn world)) (won world) (winner world))
																																  |(won world) == False && x<(-212.5) && x>(-287.5) && y>(-87.5) && y< (-12.5) && (makeMove (board world) (turn world) (1,4)) /= Nothing = (World (fromJust(makeMove (board world) (turn world) (1,4))) (other (turn world)) (won world) (winner world))
																																  |(won world) == False && x<(-212.5) && x>(-287.5) && y>(-187.5) && y< (-112.5) && (makeMove (board world) (turn world) (1,5)) /= Nothing = (World (fromJust(makeMove (board world) (turn world) (1,5))) (other (turn world)) (won world) (winner world))
																																  |(won world) == False && x<(-212.5) && x>(-287.5) && y>(-287.5) && y< (-212.5) && (makeMove (board world) (turn world) (1,6)) /= Nothing = (World (fromJust(makeMove (board world) (turn world) (1,6))) (other (turn world)) (won world) (winner world))

																																  |(won world) == False && x<(-112.5) && x>(-187.5) && y>212.5 && y< 287.5 && (makeMove (board world) (turn world) (2,1)) /= Nothing = (World (fromJust(makeMove (board world) (turn world) (2,1))) (other (turn world)) (won world) (winner world))
																																  |(won world) == False && x<(-112.5) && x>(-187.5) && y>112.5 && y< 187.5 && (makeMove (board world) (turn world) (2,2)) /= Nothing = (World (fromJust(makeMove (board world) (turn world) (2,2))) (other (turn world)) (won world) (winner world))
																																  |(won world) == False && x<(-112.5) && x>(-187.5) && y>12.5 && y< 87.5 && (makeMove (board world) (turn world) (2,3)) /= Nothing = (World (fromJust(makeMove (board world) (turn world) (2,3))) (other (turn world)) (won world) (winner world))
																																  |(won world) == False && x<(-112.5) && x>(-187.5) && y>(-87.5) && y< (-12.5) && (makeMove (board world) (turn world) (2,4)) /= Nothing = (World (fromJust(makeMove (board world) (turn world) (2,4))) (other (turn world)) (won world) (winner world))
																																  |(won world) == False && x<(-112.5) && x>(-187.5) && y>(-187.5) && y< (-112.5) && (makeMove (board world) (turn world) (2,5)) /= Nothing = (World (fromJust(makeMove (board world) (turn world) (2,5))) (other (turn world)) (won world) (winner world))
																																  |(won world) == False && x<(-112.5) && x>(-187.5) && y>(-287.5) && y< (-212.5) && (makeMove (board world) (turn world) (2,6)) /= Nothing = (World (fromJust(makeMove (board world) (turn world) (2,6))) (other (turn world)) (won world) (winner world))

																																  |(won world) == False && x<(-12.5) && x>(-87.5) && y>212.5 && y< 287.5 && (makeMove (board world) (turn world) (3,1)) /= Nothing = (World (fromJust(makeMove (board world) (turn world) (3,1))) (other (turn world)) (won world) (winner world))
																																  |(won world) == False && x<(-12.5) && x>(-87.5) && y>112.5 && y< 187.5 && (makeMove (board world) (turn world) (3,2)) /= Nothing = (World (fromJust(makeMove (board world) (turn world) (3,2))) (other (turn world)) (won world) (winner world))
																																  |(won world) == False && x<(-12.5) && x>(-87.5) && y>12.5 && y< 87.5 && (makeMove (board world) (turn world) (3,3)) /= Nothing = (World (fromJust(makeMove (board world) (turn world) (3,3))) (other (turn world)) (won world) (winner world))
																																  |(won world) == False && x<(-12.5) && x>(-87.5) && y>(-87.5) && y< (-12.5) && (makeMove (board world) (turn world) (3,4)) /= Nothing = (World (fromJust(makeMove (board world) (turn world) (3,4))) (other (turn world)) (won world) (winner world))
																																  |(won world) == False && x<(-12.5) && x>(-87.5) && y>(-187.5) && y< (-112.5) && (makeMove (board world) (turn world) (3,5)) /= Nothing = (World (fromJust(makeMove (board world) (turn world) (3,5))) (other (turn world)) (won world) (winner world))
																																  |(won world) == False && x<(-12.5) && x>(-87.5) && y>(-287.5) && y< (-212.5) && (makeMove (board world) (turn world) (3,6)) /= Nothing = (World (fromJust(makeMove (board world) (turn world) (3,6))) (other (turn world)) (won world) (winner world))

																																  |(won world) == False && x<87.5 && x>12.5 && y>212.5 && y< 287.5 && (makeMove (board world) (turn world) (4,1)) /= Nothing = (World (fromJust(makeMove (board world) (turn world) (4,1))) (other (turn world)) (won world) (winner world))
																																  |(won world) == False && x<87.5 && x>12.5 && y>112.5 && y< 187.5 && (makeMove (board world) (turn world) (4,2)) /= Nothing = (World (fromJust(makeMove (board world) (turn world) (4,2))) (other (turn world)) (won world) (winner world))
																																  |(won world) == False && x<87.5 && x>12.5 && y>12.5 && y< 87.5 && (makeMove (board world) (turn world) (4,3)) /= Nothing = (World (fromJust(makeMove (board world) (turn world) (4,3))) (other (turn world)) (won world) (winner world))
																																  |(won world) == False && x<87.5 && x>12.5 && y>(-87.5) && y< (-12.5) && (makeMove (board world) (turn world) (4,4)) /= Nothing = (World (fromJust(makeMove (board world) (turn world) (4,4))) (other (turn world)) (won world) (winner world))
																																  |(won world) == False && x<87.5 && x>12.5 && y>(-187.5) && y< (-112.5) && (makeMove (board world) (turn world) (4,5)) /= Nothing = (World (fromJust(makeMove (board world) (turn world) (4,5))) (other (turn world)) (won world) (winner world))
																																  |(won world) == False && x<87.5 && x>12.5 && y>(-287.5) && y< (-212.5) && (makeMove (board world) (turn world) (4,6)) /= Nothing = (World (fromJust(makeMove (board world) (turn world) (4,6))) (other (turn world)) (won world) (winner world))

																																  |(won world) == False && x<187.5 && x>112.5 && y>212.5 && y< 287.5 && (makeMove (board world) (turn world) (5,1)) /= Nothing = (World (fromJust(makeMove (board world) (turn world) (5,1))) (other (turn world)) (won world) (winner world))
																																  |(won world) == False && x<187.5 && x>112.5 && y>112.5 && y< 187.5 && (makeMove (board world) (turn world) (5,2)) /= Nothing = (World (fromJust(makeMove (board world) (turn world) (5,2))) (other (turn world)) (won world) (winner world))
																																  |(won world) == False && x<187.5 && x>112.5 && y>12.5 && y< 87.5 && (makeMove (board world) (turn world) (5,3)) /= Nothing = (World (fromJust(makeMove (board world) (turn world) (5,3))) (other (turn world)) (won world) (winner world))
																																  |(won world) == False && x<187.5 && x>112.5 && y>(-87.5) && y< (-12.5) && (makeMove (board world) (turn world) (5,4)) /= Nothing = (World (fromJust(makeMove (board world) (turn world) (5,4))) (other (turn world)) (won world) (winner world))
																																  |(won world) == False && x<187.5 && x>112.5 && y>(-187.5) && y< (-112.5) && (makeMove (board world) (turn world) (5,5)) /= Nothing = (World (fromJust(makeMove (board world) (turn world) (5,5))) (other (turn world)) (won world) (winner world))
																																  |(won world) == False && x<187.5 && x>112.5 && y>(-287.5) && y< (-212.5) && (makeMove (board world) (turn world) (5,6)) /= Nothing = (World (fromJust(makeMove (board world) (turn world) (5,6))) (other (turn world)) (won world) (winner world))

																																  |(won world) == False && x<287.5 && x>212.5 && y>212.5 && y< 287.5 && (makeMove (board world) (turn world) (6,1)) /= Nothing = (World (fromJust(makeMove (board world) (turn world) (6,1))) (other (turn world)) (won world) (winner world))
																																  |(won world) == False && x<287.5 && x>212.5 && y>112.5 && y< 187.5 && (makeMove (board world) (turn world) (6,2)) /= Nothing = (World (fromJust(makeMove (board world) (turn world) (6,2))) (other (turn world)) (won world) (winner world))
																																  |(won world) == False && x<287.5 && x>212.5 && y>12.5 && y< 87.5 && (makeMove (board world) (turn world) (6,3)) /= Nothing = (World (fromJust(makeMove (board world) (turn world) (6,3))) (other (turn world)) (won world) (winner world))
																																  |(won world) == False && x<287.5 && x>212.5 && y>(-87.5) && y< (-12.5) && (makeMove (board world) (turn world) (6,4)) /= Nothing = (World (fromJust(makeMove (board world) (turn world) (6,4))) (other (turn world)) (won world) (winner world))
																																  |(won world) == False && x<287.5 && x>212.5 && y>(-187.5) && y< (-112.5) && (makeMove (board world) (turn world) (6,5)) /= Nothing = (World (fromJust(makeMove (board world) (turn world) (6,5))) (other (turn world)) (won world) (winner world))
																																  |(won world) == False && x<287.5 && x>212.5 && y>(-287.5) && y< (-212.5) && (makeMove (board world) (turn world) (6,6)) /= Nothing = (World (fromJust(makeMove (board world) (turn world) (6,6))) (other (turn world)) (won world) (winner world))
																																  |otherwise = world


--handleInput (EventKey (Char k) Down _ _) world = trace ("Key " ++ show k ++ " down") world
--handleInput (EventKey (Char k) Up _ _) world = trace ("Key " ++ show k ++ " up") world
handleInput e world = world

{- Hint: when the 'World' is in a state where it is the human player's
 turn to move, a mouse press event should calculate which board position
 a click refers to, and update the board accordingly.

 At first, it is reasonable to assume that both players are human players.
-}
