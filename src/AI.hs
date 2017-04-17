-- |AI. Everything to do with the AI gets done in here.
module AI where

import Board
import Debug.Trace
import Data.Maybe
import Data.Tuple.Select
import Data.List
import Data.Ord

-- |Data tpye for game-tress.
data GameTree = GameTree { game_board :: Board, -- ^The board at the level.
                           game_turn :: Col, -- ^Whos turn it is.
                           next_moves :: [(Position, GameTree)] -- ^The next possible moves for this colour, and the trees that they produce.
                         }

-- |Get all the possible positions on the board.
possiblePositions :: Board -- ^The board.
                            -> [(Int, Int)] -- ^List of all positions.
possiblePositions board = [(a, b) |a <- [1,2..(size board)], b <-[1,2..(size board)]]

-- |Gets all the empty positions on the board.
spaces :: Board -- ^The current board.
                -> [Position] -- ^List of empty positions.
spaces board = let fil x = checkPositionForPiece x board == False in filter fil (possiblePositions board)

-- |Generate List of Positions.
gen :: Board -- ^The current board.
            -> Col -- ^The colour of whose turn it is.
            -> [Position] -- ^List of free Positions.
gen board col = spaces board

-- |Given a function to generate plausible moves (i.e. board positions) for a player (Col) on a particular board, generate a (potentially) infinite game tree.
buildTree :: (Board -> Col -> [Position]) -- ^ Move generator.
             -> Board -- ^ board state.
             -> Col -- ^ player to play next.
             -> GameTree -- ^resulting game tree.
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

-- |Add moves to the current list of possible moves that have been evaluated.
addMovesToList :: GameTree -- ^The game tree with the boards possible moves.
                          -> [(Int, Position)] -- ^The current list of moves.
                          -> [(Int,Position)] -- ^The appended list of moves.
addMovesToList tree moves = moves ++ [((evaluate (game_board (sel2 x)) (game_turn tree)), sel1 x) | x <- (next_moves tree)]

-- |Going through teh tree and returning a list of evaluated moves.
treeTraverse :: Int -- ^The maximum depth to traverse the tree.
                    -> Int -- ^The current depth.
                    -> [(Int,Position)] -- ^The list of possible moves.
                    -> GameTree -- ^The current Game-tree.
                    -> [(Int,Position)] -- ^The list of evaluated moves.
treeTraverse maxDepth depth moves tree |depth == 1 =(treeTraverse maxDepth 2 (addMovesToList tree moves) (sel2(head (next_moves tree))) )++ (treeTraverse maxDepth 2  (addMovesToList tree moves) (sel2 ((next_moves tree)!!1)) )
                                    --  |depth == 2 = (treeTraverse 3 moves (sel2(head (next_moves tree))) ++ treeTraverse 3 moves (sel2((next_moves tree)!! 1)))
                                    --  |depth == 3 = trace ("depth 3") (treeTraverse 4  (addMovesToList tree moves) (sel2 (head (next_moves tree)))) ++ (treeTraverse 4  (addMovesToList tree moves) (sel2 ((next_moves tree)!!1)))
                                       |otherwise = moves

-- |Get the best next move from a (possibly infinite) game tree.
getBestMove :: Int -- ^ Maximum search depth.
               -> GameTree -- ^ Initial game tree.
               -> Position -- ^The best moves available.
getBestMove depth tree |length (next_moves tree) > 1 = sel2(maximumFromList (treeTraverse depth 1 [] tree))
            		       |otherwise = sel1 (head (next_moves tree))

-- |Sorts the list of evaluated moves, and returns the best one.
maximumFromList :: [(Int,Position)] -- ^The list of evaluated moves.
                                    -> (Int,Position) -- ^The best possible move.
maximumFromList list = (last ((sortBy (comparing $ fst) list)))

-- |Checks if the game has been won, if not then if it is the AIs turn then it makes a move.
updateWorld :: Float -- ^ Time since last update.
            -> World -- ^ Current world state.
            -> IO World -- ^Returns the IO world, either unchanged, or with the move that the AI has made.
updateWorld t world |length (pieces (board world)) > 4 && checkWon (board world) == Just Black = do return (World (board world) (turn world) (True) (Black) False False 10 False (pics world) (squares world) (counterOne world) (counterTwo world))
            		    |length (pieces (board world)) > 4 && checkWon (board world) == Just White = do return (World (board world) (turn world) (True) (White) False False 10 False (pics world) (squares world) (counterOne world) (counterTwo world))
            		    |(turn world) == White = do return (World (fromJust(makeMove (board world) (turn world) (getBestMove 1 (buildTree (gen) (board world) (turn world))))) (other (turn world)) (won world) (winner world) False (pause world) 10 (time world) (pics world) (squares world) (counterOne world) (counterTwo world))
                    |time world == True && pause world == True = do return (world)
                    |time world == True && timer world < 0 = do return (World (fromJust(makeMove (board world) (turn world) (getBestMove 1 (buildTree (gen) (board world) (turn world))))) (other (turn world)) (won world) (winner world) False (pause world) 10 (time world) (pics world) (squares world) (counterOne world) (counterTwo world))
                    |time world == True = do return (World (board world) (turn world) (won world) (winner world) (hint world) (pause world) ((timer world)-0.1) (time world) (pics world) (squares world) (counterOne world) (counterTwo world))
                    |otherwise = do return (world)
