module AI where

import Board

import Debug.Trace
import Data.Maybe
import Data.Tuple.Select
import Data.List
import Data.Ord

data GameTree = GameTree { game_board :: Board,
                           game_turn :: Col,
                           next_moves :: [(Position, GameTree)] }

possiblePositions :: Board -> [(Int, Int)]
possiblePositions board = [(a, b) |a <- [1,2..(size board)], b <-[1,2..(size board)]]

spaces :: Board -> [Position]
spaces board = let fil x = checkPositionForPiece x board == False in filter fil (possiblePositions board)

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



addMovesToList :: GameTree -> [(Int, Position)] -> [(Int,Position)]
addMovesToList tree moves = moves ++ [((evaluate (game_board (sel2 x)) (game_turn tree)), sel1 x) | x <- (next_moves tree)]

treeTraverse :: Int -> Int -> [(Int,Position)] -> GameTree -> [(Int,Position)]
treeTraverse maxDepth depth moves tree |depth == 1 =(treeTraverse maxDepth 2 (addMovesToList tree moves) (sel2(head (next_moves tree))) )++ (treeTraverse maxDepth 2  (addMovesToList tree moves) (sel2 ((next_moves tree)!!1)) )
                                    --  |depth == 2 = (treeTraverse 3 moves (sel2(head (next_moves tree))) ++ treeTraverse 3 moves (sel2((next_moves tree)!! 1)))
                                    --  |depth == 3 = trace ("depth 3") (treeTraverse 4  (addMovesToList tree moves) (sel2 (head (next_moves tree)))) ++ (treeTraverse 4  (addMovesToList tree moves) (sel2 ((next_moves tree)!!1)))
                                       |otherwise = moves

-- Get the best next move from a (possibly infinite) game tree. This should
-- traverse the game tree up to a certain depth, and pick the move which
-- leads to the position with the best score for the player whose turn it
-- is at the top of the game tree.
getBestMove :: Int -- ^ Maximum search depth
               -> GameTree -- ^ Initial game tree
               -> Position
getBestMove depth tree |length (next_moves tree) > 1 = sel2(maximumFromList (treeTraverse depth 1 [] tree))
            		       |otherwise = sel1 (head (next_moves tree))


maximumFromList :: [(Int,Position)] -> (Int,Position)
maximumFromList list = (last ((sortBy (comparing $ fst) list)))

{-
maximumFromList :: [(Int,Position)] -> (Int,Position)
maximumFromList list | length list == 35 =  (last (shuffle (sortBy (comparing $ fst) list) [1]))
                     | otherwise = last(sortBy (comparing $ fst) list)-}

-- Update the world state after some time has passed
updateWorld :: Float -- ^ time since last update (you can ignore this)
            -> World -- ^ current world state
            -> World
updateWorld t world |length (pieces (board world)) > 4 && checkWon (board world) == Just Black = (World (board world) (turn world) (True) (Black))
            		    |length (pieces (board world)) > 4 && checkWon (board world) == Just White = (World (board world) (turn world) (True) (White))
            		    |(turn world) == White = (World (fromJust(makeMove (board world) (turn world) (getBestMove 1 (buildTree (gen) (board world) (turn world))))) (other (turn world)) (won world) (winner world))
            		    |otherwise = world


{-updateWorld t world |length (pieces (board world)) > 4 && checkWon (board world) == Just Black = (World (board world) (turn world) (True) (Black))
            		    |length (pieces (board world)) > 4 && checkWon (board world) == Just White = (World (board world) (turn world) (True) (White))
            		    |otherwise = world-}


{- Hint: 'updateWorld' is where the AI gets called. If the world state
 indicates that it is a computer player's turn, updateWorld should use
 'getBestMove' to find where the computer player should play, and update
 the board in the world state with that move.

 In a complete implementation, 'updateWorld' should also check if either
 player has won and display a message if so.
-}
