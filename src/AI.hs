module AI where

import Board

import Debug.Trace
import Data.Maybe
import Data.Tuple.Select

data GameTree = GameTree { game_board :: Board,
                           game_turn :: Col,
                           next_moves :: [(Position, GameTree)] }

possiblePositions = [(a, b) |a <- [1,2,3,4,5,6], b <-[1,2,3,4,5,6]]

spaces :: Board -> [Position]
spaces board = let fil x = checkPositionForPiece x board == False in filter fil possiblePositions

gen :: Board -> Col -> [Position]
gen board col = spaces board

-- Given a function to generate plausible moves (i.e. board positions)
-- for a player (Col) on a particular board, generate a (potentially)
-- infinite game tree.
--
-- (It's not actually infinite since the board is finite, but it's sufficiently
-- big that you might as well consider it infinite!)
--
-- An important part of the AI is the 'gen' function you pass in here.
-- Rather than generating every possible move (which would result in an
-- unmanageably large game tree!) it could, for example, generate moves
-- according to various simpler strategies.
buildTree :: (Board -> Col -> [Position]) -- ^ Move generator
             -> Board -- ^ board state
             -> Col -- ^ player to play next
             -> GameTree
buildTree gen b c = let moves = gen b c in -- generated moves
                        GameTree b c (mkNextStates moves)
  where
    mkNextStates :: [Position] -> [(Position, GameTree)]
    mkNextStates [] = []
    mkNextStates (pos : xs)
        = case makeMove b c pos of -- try making the suggested move
               Nothing -> mkNextStates xs -- not successful, no new state
               Just b' -> (pos, buildTree gen b' (other c)) : mkNextStates xs
                             -- successful, make move and build tree from 
                             -- here for opposite player

-- Get the best next move from a (possibly infinite) game tree. This should
-- traverse the game tree up to a certain depth, and pick the move which
-- leads to the position with the best score for the player whose turn it
-- is at the top of the game tree.
getBestMove :: Int -- ^ Maximum search depth
               -> GameTree -- ^ Initial game tree
               -> Position
getBestMove max tree |length (next_moves tree) >34 = sel1 ((next_moves tree) !! 29)
		     |length (next_moves tree) >32 = sel1 ((next_moves tree) !! 2)
		     |length (next_moves tree) >30 = sel1 ((next_moves tree) !! 15)
		     |length (next_moves tree) >28 = sel1 ((next_moves tree) !! 22)
		     |length (next_moves tree) >26 = sel1 ((next_moves tree) !! 4)
		     |length (next_moves tree) >24 = sel1 ((next_moves tree) !! 23)
		     |length (next_moves tree) >22 = sel1 ((next_moves tree) !! 19)
		     |length (next_moves tree) >20 = sel1 ((next_moves tree) !! 5)
		     |length (next_moves tree) >15 = sel1 ((next_moves tree) !! 13)
		     |length (next_moves tree) >13 = sel1 ((next_moves tree) !! 12)
		     |length (next_moves tree) >10 = sel1 ((next_moves tree) !! 9)
		     |length (next_moves tree) >6 = sel1 ((next_moves tree) !! 6)
		     |length (next_moves tree) >3 = sel1 ((next_moves tree) !! 1)
		     |otherwise = sel1 (head (next_moves tree))


-- Update the world state after some time has passed
updateWorld :: Float -- ^ time since last update (you can ignore this)
            -> World -- ^ current world state
            -> World
updateWorld t world |length (pieces (board world)) > 4 && checkWon (board world) == Just Black = trace ("Black has Won!") (World (board world) (turn world) (True) (Black))
		    |length (pieces (board world)) > 4 && checkWon (board world) == Just White = trace ("White has Won!") (World (board world) (turn world) (True) (White))
		    |(turn world) == White = (World (fromJust(makeMove (board world) (turn world) (getBestMove 1 (buildTree (gen) (board world) (turn world))))) (other (turn world)) (won world) (winner world))
		    |otherwise = world


{- Hint: 'updateWorld' is where the AI gets called. If the world state
 indicates that it is a computer player's turn, updateWorld should use
 'getBestMove' to find where the computer player should play, and update
 the board in the world state with that move.

 At first, it is reasonable for this to be a random move!

 If both players are human players, the simple version above will suffice,
 since it does nothing.

 In a complete implementation, 'updateWorld' should also check if either 
 player has won and display a message if so.
-}


