module Board where

import Data.Tuple.Select
import Debug.Trace
import Data.Maybe

data Col = Black | White | Empty
  deriving (Show, Eq)

other :: Col -> Col
other Black = White
other White = Black

type Position = (Int, Int)


-- A Board is a record containing the board size (a board is a square grid,
-- n * n), the number of pieces in a row required to win, and a list
-- of pairs of position and the colour at that position.  So a 10x10 board
-- for a game of 5 in a row with a black piece at 5,5 and a white piece at 8,7
-- would be represented as:
--
-- Board 10 5 [((5, 5), Black), ((8,7), White)]

data Board = Board { size :: Int,
                     target :: Int,
                     pieces :: [(Position, Col)]
                   }
  deriving (Show, Eq)

-- Default board is 6x6, target is 3 in a row, no initial pieces
initBoard = Board 6 3 []

-- Overall state is the board and whose turn it is, plus any further
-- information about the world (this may later include, for example, player
-- names, timers, information about rule variants, etc)
--
-- Feel free to extend this, and 'Board' above with anything you think
-- will be useful (information for the AI, for example, such as where the
-- most recent moves were).
data World = World { board :: Board,
                     turn :: Col,
            		     won :: Bool,
            		     winner :: Col }

initWorld = World initBoard Black False Empty

-- Play a move on the board; return 'Nothing' if the move is invalid
-- (e.g. outside the range of the board, or there is a piece already there)
makeMove :: Board -> Col -> Position -> Maybe Board
makeMove board col pos |checkPositionForPiece pos board == False && checkPositionOnBoard board pos == True = Just (Board (size board) (target board) ([(pos, col)]++(pieces board)))
            		       |otherwise = Nothing

restartGame :: World -> World
restartGame world = (World initBoard Black False Empty)

increaseBoardSize :: World -> World
increaseBoardSize world |size (board world)< 19 = (World (Board (size (board world)+1) (target (board world)) []) Black False Empty)
                        |otherwise = world

decreaseBoardSize :: World -> World
decreaseBoardSize world |size (board world)> 6 = (World (Board (size (board world)-1) (target (board world)) []) Black False Empty)
                        |otherwise = world


increaseLineSize :: World -> World
increaseLineSize world |target (board world) == 3 = (World (Board (size (board world)) (5) []) Black False Empty)
                       |otherwise = world

decreaseLineSize :: World -> World
decreaseLineSize world |target (board world) == 5 = (World (Board (size (board world)) (3) []) Black False Empty)
                       |otherwise = world

undoMove :: World -> World
undoMove world = do
        let newBoard =  (Board (size (board world)) (target (board world)) (tail(tail(pieces (board world)))))
        World (newBoard) (turn world) (won world) (winner world)

checkPositionOnBoard :: Board -> Position -> Bool
checkPositionOnBoard board pos = elem pos piecesOnBoard
		       where piecesOnBoard = (positions board)

checkPositionForPiece :: Position -> Board -> Bool
checkPositionForPiece pos board = elem pos (map (\ posi -> sel1  posi) piecesOnBoard)
		          where piecesOnBoard = pieces board

positions :: Board -> [(Int, Int)]
positions board = [(a, b) |a <- [1,2..(size board)], b <-[1,2..(size board)]]


getPositionColor :: Board -> Position -> Col
getPositionColor board pos | checkPositionOnBoard board pos == True && checkPositionForPiece pos board == True = sel2 (head[x | x <- pieces board, sel1 x == pos])
			                     | otherwise = Empty


-- Check whether the board is in a winning state for either player.
-- Returns 'Nothing' if neither player has won yet
-- Returns 'Just c' if the player 'c' has won
checkWon :: Board -> Maybe Col
checkWon board |checkEast board (sel1 (head (pieces board))) (getPositionColor board (sel1 (head (pieces board)))) (target board) == True = Just (getPositionColor board (sel1 (head (pieces board))))
               |checkWest board (sel1 (head (pieces board))) (getPositionColor board (sel1 (head (pieces board)))) (target board) == True = Just (getPositionColor board (sel1 (head (pieces board))))
               |checkMiddleRow board (sel1 (head (pieces board))) (getPositionColor board (sel1 (head (pieces board)))) (target board) == True = Just (getPositionColor board (sel1 (head (pieces board))))
	             |checkNorth board (sel1 (head (pieces board))) (getPositionColor board (sel1 (head (pieces board)))) (target board) == True = Just (getPositionColor board (sel1 (head (pieces board))))
               |checkSouth board (sel1 (head (pieces board))) (getPositionColor board (sel1 (head (pieces board)))) (target board) == True = Just (getPositionColor board (sel1 (head (pieces board))))
               |checkMiddleColumn board (sel1 (head (pieces board))) (getPositionColor board (sel1 (head (pieces board)))) (target board) == True = Just (getPositionColor board (sel1 (head (pieces board))))
	             |checkNorthEast board (sel1 (head (pieces board))) (getPositionColor board (sel1 (head (pieces board)))) (target board) == True = Just (getPositionColor board (sel1 (head (pieces board))))
               |checkSouthWest board (sel1 (head (pieces board))) (getPositionColor board (sel1 (head (pieces board)))) (target board) == True = Just (getPositionColor board (sel1 (head (pieces board))))
               |checkNorthWest board (sel1 (head (pieces board))) (getPositionColor board (sel1 (head (pieces board)))) (target board) == True = Just (getPositionColor board (sel1 (head (pieces board))))
               |checkSouthEast board (sel1 (head (pieces board))) (getPositionColor board (sel1 (head (pieces board)))) (target board) == True = Just (getPositionColor board (sel1 (head (pieces board))))
               |checkMiddleDiagonalOne board (sel1 (head (pieces board))) (getPositionColor board (sel1 (head (pieces board)))) (target board) == True = Just (getPositionColor board (sel1 (head (pieces board))))
               |checkMiddleDiagonalTwo board (sel1 (head (pieces board))) (getPositionColor board (sel1 (head (pieces board)))) (target board) == True = Just (getPositionColor board (sel1 (head (pieces board))))
	             |otherwise = Nothing

checkEast :: Board -> Position -> Col -> Int -> Bool
checkEast board pos col target |target == 3 && checkPositionOnBoard board (sel1 pos -1, sel2 pos) == True && checkPositionForPiece (sel1 pos -1, sel2 pos) board == True && getPositionColor board (sel1 pos -1, sel2 pos) == col && checkPositionOnBoard board (sel1 pos +1, sel2 pos) == True && checkPositionForPiece (sel1 pos +1, sel2 pos) board == True && getPositionColor board (sel1 pos +1, sel2 pos) == col && checkPositionOnBoard board (sel1 pos +2, sel2 pos) == True && checkPositionForPiece (sel1 pos +2, sel2 pos) board == True && getPositionColor board (sel1 pos +2, sel2 pos) == col = False
                               |target == 3 && checkPositionOnBoard board (sel1 pos +3, sel2 pos) == True && checkPositionForPiece (sel1 pos +3, sel2 pos) board == True && getPositionColor board (sel1 pos +3, sel2 pos) == col && checkPositionOnBoard board (sel1 pos +1, sel2 pos) == True && checkPositionForPiece (sel1 pos +1, sel2 pos) board == True && getPositionColor board (sel1 pos +1, sel2 pos) == col && checkPositionOnBoard board (sel1 pos +2, sel2 pos) == True && checkPositionForPiece (sel1 pos +2, sel2 pos) board == True && getPositionColor board (sel1 pos +2, sel2 pos) == col = False
                               |target == 3 && checkPositionOnBoard board (sel1 pos +1, sel2 pos) == True && checkPositionForPiece (sel1 pos +1, sel2 pos) board == True && getPositionColor board (sel1 pos +1, sel2 pos) == col && checkPositionOnBoard board (sel1 pos +2, sel2 pos) == True && checkPositionForPiece (sel1 pos +2, sel2 pos) board == True && getPositionColor board (sel1 pos +2, sel2 pos) == col = True
                               |target == 5 && checkPositionOnBoard board (sel1 pos -1, sel2 pos) == True && checkPositionForPiece (sel1 pos -1, sel2 pos) board == True && getPositionColor board (sel1 pos -1, sel2 pos) == col && checkPositionOnBoard board (sel1 pos +1, sel2 pos) == True && checkPositionForPiece (sel1 pos +1, sel2 pos) board == True && getPositionColor board (sel1 pos +1, sel2 pos) == col && checkPositionOnBoard board (sel1 pos +2, sel2 pos) == True && checkPositionForPiece (sel1 pos +2, sel2 pos) board == True && getPositionColor board (sel1 pos +2, sel2 pos) == col && checkPositionOnBoard board (sel1 pos +3, sel2 pos) == True && checkPositionForPiece (sel1 pos +3, sel2 pos) board == True && getPositionColor board (sel1 pos +3, sel2 pos) == col && checkPositionOnBoard board (sel1 pos +4, sel2 pos) == True && checkPositionForPiece (sel1 pos +4, sel2 pos) board == True && getPositionColor board (sel1 pos +4, sel2 pos) == col = False
                               |target == 5 && checkPositionOnBoard board (sel1 pos +5, sel2 pos) == True && checkPositionForPiece (sel1 pos +5, sel2 pos) board == True && getPositionColor board (sel1 pos +5, sel2 pos) == col && checkPositionOnBoard board (sel1 pos +1, sel2 pos) == True && checkPositionForPiece (sel1 pos +1, sel2 pos) board == True && getPositionColor board (sel1 pos +1, sel2 pos) == col && checkPositionOnBoard board (sel1 pos +2, sel2 pos) == True && checkPositionForPiece (sel1 pos +2, sel2 pos) board == True && getPositionColor board (sel1 pos +2, sel2 pos) == col && checkPositionOnBoard board (sel1 pos +3, sel2 pos) == True && checkPositionForPiece (sel1 pos +3, sel2 pos) board == True && getPositionColor board (sel1 pos +3, sel2 pos) == col && checkPositionOnBoard board (sel1 pos +4, sel2 pos) == True && checkPositionForPiece (sel1 pos +4, sel2 pos) board == True && getPositionColor board (sel1 pos +4, sel2 pos) == col = False
                               |target == 5 && checkPositionOnBoard board (sel1 pos +1, sel2 pos) == True && checkPositionForPiece (sel1 pos +1, sel2 pos) board == True && getPositionColor board (sel1 pos +1, sel2 pos) == col && checkPositionOnBoard board (sel1 pos +2, sel2 pos) == True && checkPositionForPiece (sel1 pos +2, sel2 pos) board == True && getPositionColor board (sel1 pos +2, sel2 pos) == col && checkPositionOnBoard board (sel1 pos +3, sel2 pos) == True && checkPositionForPiece (sel1 pos +3, sel2 pos) board == True && getPositionColor board (sel1 pos +3, sel2 pos) == col && checkPositionOnBoard board (sel1 pos +4, sel2 pos) == True && checkPositionForPiece (sel1 pos +4, sel2 pos) board == True && getPositionColor board (sel1 pos +4, sel2 pos) == col = True
                               |target == 5 && checkPositionOnBoard board (sel1 pos -2, sel2 pos) == True && checkPositionForPiece (sel1 pos -2, sel2 pos) board == True && getPositionColor board (sel1 pos -2, sel2 pos) == col && checkPositionOnBoard board (sel1 pos +1, sel2 pos) == True && checkPositionForPiece (sel1 pos +1, sel2 pos) board == True && getPositionColor board (sel1 pos +1, sel2 pos) == col && checkPositionOnBoard board (sel1 pos +2, sel2 pos) == True && checkPositionForPiece (sel1 pos +2, sel2 pos) board == True && getPositionColor board (sel1 pos +2, sel2 pos) == col && checkPositionOnBoard board (sel1 pos +3, sel2 pos) == True && checkPositionForPiece (sel1 pos +3, sel2 pos) board == True && getPositionColor board (sel1 pos +3, sel2 pos) == col && checkPositionOnBoard board (sel1 pos -1, sel2 pos) == True && checkPositionForPiece (sel1 pos -1, sel2 pos) board == True && getPositionColor board (sel1 pos -1, sel2 pos) == col = False
                               |target == 5 && checkPositionOnBoard board (sel1 pos +1, sel2 pos) == True && checkPositionForPiece (sel1 pos +1, sel2 pos) board == True && getPositionColor board (sel1 pos +1, sel2 pos) == col && checkPositionOnBoard board (sel1 pos +2, sel2 pos) == True && checkPositionForPiece (sel1 pos +2, sel2 pos) board == True && getPositionColor board (sel1 pos +2, sel2 pos) == col && checkPositionOnBoard board (sel1 pos +3, sel2 pos) == True && checkPositionForPiece (sel1 pos +3, sel2 pos) board == True && getPositionColor board (sel1 pos +3, sel2 pos) == col && checkPositionOnBoard board (sel1 pos -1, sel2 pos) == True && checkPositionForPiece (sel1 pos -1, sel2 pos) board == True && getPositionColor board (sel1 pos -1, sel2 pos) == col = True
                               |otherwise = False

checkWest :: Board -> Position -> Col -> Int -> Bool
checkWest board pos col target |target == 3 && checkPositionOnBoard board (sel1 pos -3, sel2 pos) == True && checkPositionForPiece (sel1 pos -3, sel2 pos) board == True && getPositionColor board (sel1 pos -3, sel2 pos) == col && checkPositionOnBoard board (sel1 pos -2, sel2 pos) == True && checkPositionForPiece (sel1 pos -2, sel2 pos) board == True && getPositionColor board (sel1 pos -2, sel2 pos) == col && checkPositionOnBoard board (sel1 pos -1, sel2 pos) == True && checkPositionForPiece (sel1 pos -1, sel2 pos) board == True && getPositionColor board (sel1 pos -1, sel2 pos) == col = False
                               |target == 3 && checkPositionOnBoard board (sel1 pos +1, sel2 pos) == True && checkPositionForPiece (sel1 pos +1, sel2 pos) board == True && getPositionColor board (sel1 pos +1, sel2 pos) == col && checkPositionOnBoard board (sel1 pos -2, sel2 pos) == True && checkPositionForPiece (sel1 pos -2, sel2 pos) board == True && getPositionColor board (sel1 pos -2, sel2 pos) == col && checkPositionOnBoard board (sel1 pos -1, sel2 pos) == True && checkPositionForPiece (sel1 pos -1, sel2 pos) board == True && getPositionColor board (sel1 pos -1, sel2 pos) == col = False
                               |target == 3 && checkPositionOnBoard board (sel1 pos -2, sel2 pos) == True && checkPositionForPiece (sel1 pos -2, sel2 pos) board == True && getPositionColor board (sel1 pos -2, sel2 pos) == col && checkPositionOnBoard board (sel1 pos -1, sel2 pos) == True && checkPositionForPiece (sel1 pos -1, sel2 pos) board == True && getPositionColor board (sel1 pos -1, sel2 pos) == col = True
                               |target == 5 && checkPositionOnBoard board (sel1 pos +1, sel2 pos) == True && checkPositionForPiece (sel1 pos +1, sel2 pos) board == True && getPositionColor board (sel1 pos +1, sel2 pos) == col && checkPositionOnBoard board (sel1 pos -4, sel2 pos) == True && checkPositionForPiece (sel1 pos -4, sel2 pos) board == True && getPositionColor board (sel1 pos -4, sel2 pos) == col && checkPositionOnBoard board (sel1 pos -3, sel2 pos) == True && checkPositionForPiece (sel1 pos -3, sel2 pos) board == True && getPositionColor board (sel1 pos -3, sel2 pos) == col && checkPositionOnBoard board (sel1 pos -2, sel2 pos) == True && checkPositionForPiece (sel1 pos -2, sel2 pos) board == True && getPositionColor board (sel1 pos -2, sel2 pos) == col && checkPositionOnBoard board (sel1 pos -1, sel2 pos) == True && checkPositionForPiece (sel1 pos -1, sel2 pos) board == True && getPositionColor board (sel1 pos -1, sel2 pos) == col = False
                               |target == 5 && checkPositionOnBoard board (sel1 pos -5, sel2 pos) == True && checkPositionForPiece (sel1 pos -5, sel2 pos) board == True && getPositionColor board (sel1 pos -5, sel2 pos) == col && checkPositionOnBoard board (sel1 pos -4, sel2 pos) == True && checkPositionForPiece (sel1 pos -4, sel2 pos) board == True && getPositionColor board (sel1 pos -4, sel2 pos) == col && checkPositionOnBoard board (sel1 pos -3, sel2 pos) == True && checkPositionForPiece (sel1 pos -3, sel2 pos) board == True && getPositionColor board (sel1 pos -3, sel2 pos) == col && checkPositionOnBoard board (sel1 pos -2, sel2 pos) == True && checkPositionForPiece (sel1 pos -2, sel2 pos) board == True && getPositionColor board (sel1 pos -2, sel2 pos) == col && checkPositionOnBoard board (sel1 pos -1, sel2 pos) == True && checkPositionForPiece (sel1 pos -1, sel2 pos) board == True && getPositionColor board (sel1 pos -1, sel2 pos) == col = False
                               |target == 5 && checkPositionOnBoard board (sel1 pos -4, sel2 pos) == True && checkPositionForPiece (sel1 pos -4, sel2 pos) board == True && getPositionColor board (sel1 pos -4, sel2 pos) == col && checkPositionOnBoard board (sel1 pos -3, sel2 pos) == True && checkPositionForPiece (sel1 pos -3, sel2 pos) board == True && getPositionColor board (sel1 pos -3, sel2 pos) == col && checkPositionOnBoard board (sel1 pos -2, sel2 pos) == True && checkPositionForPiece (sel1 pos -2, sel2 pos) board == True && getPositionColor board (sel1 pos -2, sel2 pos) == col && checkPositionOnBoard board (sel1 pos -1, sel2 pos) == True && checkPositionForPiece (sel1 pos -1, sel2 pos) board == True && getPositionColor board (sel1 pos -1, sel2 pos) == col = True
                               |target == 5 && checkPositionOnBoard board (sel1 pos +1, sel2 pos) == True && checkPositionForPiece (sel1 pos +1, sel2 pos) board == True && getPositionColor board (sel1 pos +1, sel2 pos) == col && checkPositionOnBoard board (sel1 pos +2, sel2 pos) == True && checkPositionForPiece (sel1 pos +2, sel2 pos) board == True && getPositionColor board (sel1 pos +2, sel2 pos) == col && checkPositionOnBoard board (sel1 pos -3, sel2 pos) == True && checkPositionForPiece (sel1 pos -3, sel2 pos) board == True && getPositionColor board (sel1 pos -3, sel2 pos) == col && checkPositionOnBoard board (sel1 pos -2, sel2 pos) == True && checkPositionForPiece (sel1 pos -2, sel2 pos) board == True && getPositionColor board (sel1 pos -2, sel2 pos) == col && checkPositionOnBoard board (sel1 pos -1, sel2 pos) == True && checkPositionForPiece (sel1 pos -1, sel2 pos) board == True && getPositionColor board (sel1 pos -1, sel2 pos) == col = False
                               |target == 5 && checkPositionOnBoard board (sel1 pos +1, sel2 pos) == True && checkPositionForPiece (sel1 pos +1, sel2 pos) board == True && getPositionColor board (sel1 pos +1, sel2 pos) == col && checkPositionOnBoard board (sel1 pos -3, sel2 pos) == True && checkPositionForPiece (sel1 pos -3, sel2 pos) board == True && getPositionColor board (sel1 pos -3, sel2 pos) == col && checkPositionOnBoard board (sel1 pos -2, sel2 pos) == True && checkPositionForPiece (sel1 pos -2, sel2 pos) board == True && getPositionColor board (sel1 pos -2, sel2 pos) == col && checkPositionOnBoard board (sel1 pos -1, sel2 pos) == True && checkPositionForPiece (sel1 pos -1, sel2 pos) board == True && getPositionColor board (sel1 pos -1, sel2 pos) == col = True
                               |otherwise = False

checkMiddleRow :: Board -> Position -> Col -> Int -> Bool
checkMiddleRow board pos col target |target == 3 && checkPositionOnBoard board (sel1 pos -2, sel2 pos) == True && checkPositionForPiece (sel1 pos -2, sel2 pos) board == True && getPositionColor board (sel1 pos -2, sel2 pos) == col && checkPositionOnBoard board (sel1 pos -1, sel2 pos) == True && checkPositionForPiece (sel1 pos -1, sel2 pos) board == True && getPositionColor board (sel1 pos -1, sel2 pos) == col && checkPositionOnBoard board (sel1 pos +1, sel2 pos) == True && checkPositionForPiece (sel1 pos +1, sel2 pos) board == True && getPositionColor board (sel1 pos +1, sel2 pos) == col = False
                                    |target == 3 && checkPositionOnBoard board (sel1 pos +2, sel2 pos) == True && checkPositionForPiece (sel1 pos +2, sel2 pos) board == True && getPositionColor board (sel1 pos +2, sel2 pos) == col && checkPositionOnBoard board (sel1 pos -1, sel2 pos) == True && checkPositionForPiece (sel1 pos -1, sel2 pos) board == True && getPositionColor board (sel1 pos -1, sel2 pos) == col && checkPositionOnBoard board (sel1 pos +1, sel2 pos) == True && checkPositionForPiece (sel1 pos +1, sel2 pos) board == True && getPositionColor board (sel1 pos +1, sel2 pos) == col = False
                                    |target == 3 && checkPositionOnBoard board (sel1 pos -1, sel2 pos) == True && checkPositionForPiece (sel1 pos -1, sel2 pos) board == True && getPositionColor board (sel1 pos -1, sel2 pos) == col && checkPositionOnBoard board (sel1 pos +1, sel2 pos) == True && checkPositionForPiece (sel1 pos +1, sel2 pos) board == True && getPositionColor board (sel1 pos +1, sel2 pos) == col = True
                                    |target == 5 && checkPositionOnBoard board (sel1 pos +2, sel2 pos) == True && checkPositionForPiece (sel1 pos +2, sel2 pos) board == True && getPositionColor board (sel1 pos +2, sel2 pos) == col && checkPositionOnBoard board (sel1 pos -1, sel2 pos) == True && checkPositionForPiece (sel1 pos -1, sel2 pos) board == True && getPositionColor board (sel1 pos -1, sel2 pos) == col && checkPositionOnBoard board (sel1 pos +1, sel2 pos) == True && checkPositionForPiece (sel1 pos +1, sel2 pos) board == True && getPositionColor board (sel1 pos +1, sel2 pos) == col && checkPositionOnBoard board (sel1 pos +2, sel2 pos) == True && checkPositionForPiece (sel1 pos +2, sel2 pos) board == True && getPositionColor board (sel1 pos +2, sel2 pos) == col && checkPositionOnBoard board (sel1 pos -3, sel2 pos) == True && checkPositionForPiece (sel1 pos -3, sel2 pos) board == True && getPositionColor board (sel1 pos -3, sel2 pos) == col = False
                                    |target == 5 && checkPositionOnBoard board (sel1 pos +2, sel2 pos) == True && checkPositionForPiece (sel1 pos +2, sel2 pos) board == True && getPositionColor board (sel1 pos +2, sel2 pos) == col && checkPositionOnBoard board (sel1 pos -1, sel2 pos) == True && checkPositionForPiece (sel1 pos -1, sel2 pos) board == True && getPositionColor board (sel1 pos -1, sel2 pos) == col && checkPositionOnBoard board (sel1 pos +1, sel2 pos) == True && checkPositionForPiece (sel1 pos +1, sel2 pos) board == True && getPositionColor board (sel1 pos +1, sel2 pos) == col && checkPositionOnBoard board (sel1 pos +2, sel2 pos) == True && checkPositionForPiece (sel1 pos +2, sel2 pos) board == True && getPositionColor board (sel1 pos +2, sel2 pos) == col && checkPositionOnBoard board (sel1 pos +3, sel2 pos) == True && checkPositionForPiece (sel1 pos +3, sel2 pos) board == True && getPositionColor board (sel1 pos +3, sel2 pos) == col = False
                                    |target == 5 && checkPositionOnBoard board (sel1 pos +2, sel2 pos) == True && checkPositionForPiece (sel1 pos +2, sel2 pos) board == True && getPositionColor board (sel1 pos +2, sel2 pos) == col && checkPositionOnBoard board (sel1 pos -1, sel2 pos) == True && checkPositionForPiece (sel1 pos -1, sel2 pos) board == True && getPositionColor board (sel1 pos -1, sel2 pos) == col && checkPositionOnBoard board (sel1 pos +1, sel2 pos) == True && checkPositionForPiece (sel1 pos +1, sel2 pos) board == True && getPositionColor board (sel1 pos +1, sel2 pos) == col && checkPositionOnBoard board (sel1 pos +2, sel2 pos) == True && checkPositionForPiece (sel1 pos +2, sel2 pos) board == True && getPositionColor board (sel1 pos +2, sel2 pos) == col = True
                                    |otherwise = False

checkNorth :: Board -> Position -> Col -> Int -> Bool
checkNorth board pos col target |target == 3 && checkPositionOnBoard board (sel1 pos, sel2 pos -3) == True && checkPositionForPiece (sel1 pos, sel2 pos -3) board == True && getPositionColor board (sel1 pos, sel2 pos -3) == col && checkPositionOnBoard board (sel1 pos, sel2 pos -2) == True && checkPositionForPiece (sel1 pos, sel2 pos -2) board == True && getPositionColor board (sel1 pos, sel2 pos -2) == col && checkPositionOnBoard board (sel1 pos, sel2 pos -1) == True && checkPositionForPiece (sel1 pos, sel2 pos -1) board == True && getPositionColor board (sel1 pos, sel2 pos -1) == col = False
                                |target == 3 && checkPositionOnBoard board (sel1 pos, sel2 pos +1) == True && checkPositionForPiece (sel1 pos, sel2 pos +1) board == True && getPositionColor board (sel1 pos, sel2 pos +1) == col && checkPositionOnBoard board (sel1 pos, sel2 pos -2) == True && checkPositionForPiece (sel1 pos, sel2 pos -2) board == True && getPositionColor board (sel1 pos, sel2 pos -2) == col && checkPositionOnBoard board (sel1 pos, sel2 pos -1) == True && checkPositionForPiece (sel1 pos, sel2 pos -1) board == True && getPositionColor board (sel1 pos, sel2 pos -1) == col = False
                                |target == 3 && checkPositionOnBoard board (sel1 pos, sel2 pos -2) == True && checkPositionForPiece (sel1 pos, sel2 pos -2) board == True && getPositionColor board (sel1 pos, sel2 pos -2) == col && checkPositionOnBoard board (sel1 pos, sel2 pos -1) == True && checkPositionForPiece (sel1 pos, sel2 pos -1) board == True && getPositionColor board (sel1 pos, sel2 pos -1) == col = True
                                |otherwise = False

checkSouth :: Board -> Position -> Col -> Int -> Bool
checkSouth board pos col target |target == 3 && checkPositionOnBoard board (sel1 pos, sel2 pos +3) == True && checkPositionForPiece (sel1 pos, sel2 pos +3) board == True && getPositionColor board (sel1 pos, sel2 pos +3) == col && checkPositionOnBoard board (sel1 pos, sel2 pos +1) == True && checkPositionForPiece (sel1 pos, sel2 pos +1) board == True && getPositionColor board (sel1 pos, sel2 pos +1) == col && checkPositionOnBoard board (sel1 pos, sel2 pos +2) == True && checkPositionForPiece (sel1 pos, sel2 pos +2) board == True && getPositionColor board (sel1 pos, sel2 pos +2) == col = False
                                |target == 3 && checkPositionOnBoard board (sel1 pos, sel2 pos -1) == True && checkPositionForPiece (sel1 pos, sel2 pos -1) board == True && getPositionColor board (sel1 pos, sel2 pos -1) == col && checkPositionOnBoard board (sel1 pos, sel2 pos +1) == True && checkPositionForPiece (sel1 pos, sel2 pos +1) board == True && getPositionColor board (sel1 pos, sel2 pos +1) == col && checkPositionOnBoard board (sel1 pos, sel2 pos +2) == True && checkPositionForPiece (sel1 pos, sel2 pos +2) board == True && getPositionColor board (sel1 pos, sel2 pos +2) == col = False
                                |target == 3 && checkPositionOnBoard board (sel1 pos, sel2 pos +1) == True && checkPositionForPiece (sel1 pos, sel2 pos +1) board == True && getPositionColor board (sel1 pos, sel2 pos +1) == col && checkPositionOnBoard board (sel1 pos, sel2 pos +2) == True && checkPositionForPiece (sel1 pos, sel2 pos +2) board == True && getPositionColor board (sel1 pos, sel2 pos +2) == col = True
                                |otherwise = False

checkMiddleColumn :: Board -> Position -> Col -> Int -> Bool
checkMiddleColumn board pos col target |target == 3 && checkPositionOnBoard board (sel1 pos, sel2 pos -2) == True && checkPositionForPiece (sel1 pos, sel2 pos -2) board == True && getPositionColor board (sel1 pos, sel2 pos -2) == col && checkPositionOnBoard board (sel1 pos, sel2 pos -1) == True && checkPositionForPiece (sel1 pos, sel2 pos -1) board == True && getPositionColor board (sel1 pos, sel2 pos -1) == col && checkPositionOnBoard board (sel1 pos, sel2 pos +1) == True && checkPositionForPiece (sel1 pos, sel2 pos +1) board == True && getPositionColor board (sel1 pos, sel2 pos +1) == col = False
                                       |target == 3 && checkPositionOnBoard board (sel1 pos, sel2 pos +2) == True && checkPositionForPiece (sel1 pos, sel2 pos +2) board == True && getPositionColor board (sel1 pos, sel2 pos +2) == col && checkPositionOnBoard board (sel1 pos, sel2 pos -1) == True && checkPositionForPiece (sel1 pos, sel2 pos -1) board == True && getPositionColor board (sel1 pos, sel2 pos -1) == col && checkPositionOnBoard board (sel1 pos, sel2 pos +1) == True && checkPositionForPiece (sel1 pos, sel2 pos +1) board == True && getPositionColor board (sel1 pos, sel2 pos +1) == col = False
                                       |target == 3 && checkPositionOnBoard board (sel1 pos, sel2 pos -1) == True && checkPositionForPiece (sel1 pos, sel2 pos -1) board == True && getPositionColor board (sel1 pos, sel2 pos -1) == col && checkPositionOnBoard board (sel1 pos, sel2 pos +1) == True && checkPositionForPiece (sel1 pos, sel2 pos +1) board == True && getPositionColor board (sel1 pos, sel2 pos +1) == col = True
                                       |otherwise = False

checkNorthEast :: Board -> Position -> Col -> Int -> Bool
checkNorthEast board pos col target |target == 3 && checkPositionOnBoard board (sel1 pos+3, sel2 pos -3) == True && checkPositionForPiece (sel1 pos+3, sel2 pos -3) board == True && getPositionColor board (sel1 pos+3, sel2 pos -3) == col && checkPositionOnBoard board (sel1 pos+1, sel2 pos -1) == True && checkPositionForPiece (sel1 pos+1, sel2 pos -1) board == True && getPositionColor board (sel1 pos+1, sel2 pos -1) == col && checkPositionOnBoard board (sel1 pos+2, sel2 pos -2) == True && checkPositionForPiece (sel1 pos+2, sel2 pos -2) board == True && getPositionColor board (sel1 pos+2, sel2 pos -2) == col = False
                                    |target == 3 && checkPositionOnBoard board (sel1 pos-1, sel2 pos +1) == True && checkPositionForPiece (sel1 pos-1, sel2 pos +1) board == True && getPositionColor board (sel1 pos-1, sel2 pos +1) == col && checkPositionOnBoard board (sel1 pos+1, sel2 pos -1) == True && checkPositionForPiece (sel1 pos+1, sel2 pos -1) board == True && getPositionColor board (sel1 pos+1, sel2 pos -1) == col && checkPositionOnBoard board (sel1 pos+2, sel2 pos -2) == True && checkPositionForPiece (sel1 pos+2, sel2 pos -2) board == True && getPositionColor board (sel1 pos+2, sel2 pos -2) == col = False
                                    |target == 3 && checkPositionOnBoard board (sel1 pos+1, sel2 pos -1) == True && checkPositionForPiece (sel1 pos+1, sel2 pos -1) board == True && getPositionColor board (sel1 pos+1, sel2 pos -1) == col && checkPositionOnBoard board (sel1 pos+2, sel2 pos -2) == True && checkPositionForPiece (sel1 pos+2, sel2 pos -2) board == True && getPositionColor board (sel1 pos+2, sel2 pos -2) == col = True
                                    |otherwise = False

checkSouthWest :: Board -> Position -> Col -> Int -> Bool
checkSouthWest board pos col target |target == 3 && checkPositionOnBoard board (sel1 pos-3, sel2 pos +3) == True && checkPositionForPiece (sel1 pos-3, sel2 pos +3) board == True && getPositionColor board (sel1 pos-3, sel2 pos +3) == col && checkPositionOnBoard board (sel1 pos-1, sel2 pos +1) == True && checkPositionForPiece (sel1 pos-1, sel2 pos +1) board == True && getPositionColor board (sel1 pos-1, sel2 pos +1) == col && checkPositionOnBoard board (sel1 pos-2, sel2 pos +2) == True && checkPositionForPiece (sel1 pos-2, sel2 pos +2) board == True && getPositionColor board (sel1 pos-2, sel2 pos +2) == col = False
                                    |target == 3 && checkPositionOnBoard board (sel1 pos+1, sel2 pos -1) == True && checkPositionForPiece (sel1 pos+1, sel2 pos -1) board == True && getPositionColor board (sel1 pos+1, sel2 pos -1) == col && checkPositionOnBoard board (sel1 pos-1, sel2 pos +1) == True && checkPositionForPiece (sel1 pos-1, sel2 pos +1) board == True && getPositionColor board (sel1 pos-1, sel2 pos +1) == col && checkPositionOnBoard board (sel1 pos-2, sel2 pos +2) == True && checkPositionForPiece (sel1 pos-2, sel2 pos +2) board == True && getPositionColor board (sel1 pos-2, sel2 pos +2) == col = False
                                    |target == 3 && checkPositionOnBoard board (sel1 pos-1, sel2 pos +1) == True && checkPositionForPiece (sel1 pos-1, sel2 pos +1) board == True && getPositionColor board (sel1 pos-1, sel2 pos +1) == col && checkPositionOnBoard board (sel1 pos-2, sel2 pos +2) == True && checkPositionForPiece (sel1 pos-2, sel2 pos +2) board == True && getPositionColor board (sel1 pos-2, sel2 pos +2) == col = True
                                    |otherwise = False

checkNorthWest :: Board -> Position -> Col -> Int -> Bool
checkNorthWest board pos col target |target == 3 && checkPositionOnBoard board (sel1 pos-3, sel2 pos -3) == True && checkPositionForPiece (sel1 pos-3, sel2 pos -3) board == True && getPositionColor board (sel1 pos-3, sel2 pos -3) == col && checkPositionOnBoard board (sel1 pos-1, sel2 pos -1) == True && checkPositionForPiece (sel1 pos-1, sel2 pos -1) board == True && getPositionColor board (sel1 pos-1, sel2 pos -1) == col && checkPositionOnBoard board (sel1 pos-2, sel2 pos -2) == True && checkPositionForPiece (sel1 pos-2, sel2 pos -2) board == True && getPositionColor board (sel1 pos-2, sel2 pos -2) == col = False
                                    |target == 3 && checkPositionOnBoard board (sel1 pos+1, sel2 pos +1) == True && checkPositionForPiece (sel1 pos+1, sel2 pos +1) board == True && getPositionColor board (sel1 pos+1, sel2 pos +1) == col && checkPositionOnBoard board (sel1 pos-1, sel2 pos -1) == True && checkPositionForPiece (sel1 pos-1, sel2 pos -1) board == True && getPositionColor board (sel1 pos-1, sel2 pos -1) == col && checkPositionOnBoard board (sel1 pos-2, sel2 pos -2) == True && checkPositionForPiece (sel1 pos-2, sel2 pos -2) board == True && getPositionColor board (sel1 pos-2, sel2 pos -2) == col = False
                                    |target == 3 && checkPositionOnBoard board (sel1 pos-1, sel2 pos -1) == True && checkPositionForPiece (sel1 pos-1, sel2 pos -1) board == True && getPositionColor board (sel1 pos-1, sel2 pos -1) == col && checkPositionOnBoard board (sel1 pos-2, sel2 pos -2) == True && checkPositionForPiece (sel1 pos-2, sel2 pos -2) board == True && getPositionColor board (sel1 pos-2, sel2 pos -2) == col = True
      		                          |otherwise = False

checkSouthEast :: Board -> Position -> Col -> Int -> Bool
checkSouthEast board pos col target |target == 3 && checkPositionOnBoard board (sel1 pos+3, sel2 pos +3) == True && checkPositionForPiece (sel1 pos+3, sel2 pos +3) board == True && getPositionColor board (sel1 pos+3, sel2 pos +3) == col && checkPositionOnBoard board (sel1 pos+1, sel2 pos +1) == True && checkPositionForPiece (sel1 pos+1, sel2 pos +1) board == True && getPositionColor board (sel1 pos+1, sel2 pos +1) == col && checkPositionOnBoard board (sel1 pos+2, sel2 pos +2) == True && checkPositionForPiece (sel1 pos+2, sel2 pos +2) board == True && getPositionColor board (sel1 pos+2, sel2 pos +2) == col = False
                                    |target == 3 && checkPositionOnBoard board (sel1 pos-1, sel2 pos -1) == True && checkPositionForPiece (sel1 pos-1, sel2 pos -1) board == True && getPositionColor board (sel1 pos-1, sel2 pos -1) == col && checkPositionOnBoard board (sel1 pos+1, sel2 pos +1) == True && checkPositionForPiece (sel1 pos+1, sel2 pos +1) board == True && getPositionColor board (sel1 pos+1, sel2 pos +1) == col && checkPositionOnBoard board (sel1 pos+2, sel2 pos +2) == True && checkPositionForPiece (sel1 pos+2, sel2 pos +2) board == True && getPositionColor board (sel1 pos+2, sel2 pos +2) == col = False
                                    |target == 3 && checkPositionOnBoard board (sel1 pos+1, sel2 pos +1) == True && checkPositionForPiece (sel1 pos+1, sel2 pos +1) board == True && getPositionColor board (sel1 pos+1, sel2 pos +1) == col && checkPositionOnBoard board (sel1 pos+2, sel2 pos +2) == True && checkPositionForPiece (sel1 pos+2, sel2 pos +2) board == True && getPositionColor board (sel1 pos+2, sel2 pos +2) == col = True
                                    |otherwise = False

checkMiddleDiagonalOne :: Board -> Position -> Col -> Int -> Bool
checkMiddleDiagonalOne board pos col target |target == 3 && checkPositionOnBoard board (sel1 pos+2, sel2 pos +2) == True && checkPositionForPiece (sel1 pos+2, sel2 pos +2) board == True && getPositionColor board (sel1 pos+2, sel2 pos +2) == col && checkPositionOnBoard board (sel1 pos+1, sel2 pos +1) == True && checkPositionForPiece (sel1 pos+1, sel2 pos +1) board == True && getPositionColor board (sel1 pos+1, sel2 pos +1) == col && checkPositionOnBoard board (sel1 pos-1, sel2 pos -1) == True && checkPositionForPiece (sel1 pos-1, sel2 pos -1) board == True && getPositionColor board (sel1 pos-1, sel2 pos -1) == col = False
                                            |target == 3 && checkPositionOnBoard board (sel1 pos-2, sel2 pos -2) == True && checkPositionForPiece (sel1 pos-2, sel2 pos -2) board == True && getPositionColor board (sel1 pos-2, sel2 pos -2) == col && checkPositionOnBoard board (sel1 pos+1, sel2 pos +1) == True && checkPositionForPiece (sel1 pos+1, sel2 pos +1) board == True && getPositionColor board (sel1 pos+1, sel2 pos +1) == col && checkPositionOnBoard board (sel1 pos-1, sel2 pos -1) == True && checkPositionForPiece (sel1 pos-1, sel2 pos -1) board == True && getPositionColor board (sel1 pos-1, sel2 pos -1) == col = False
                                            |target == 3 && checkPositionOnBoard board (sel1 pos+1, sel2 pos +1) == True && checkPositionForPiece (sel1 pos+1, sel2 pos +1) board == True && getPositionColor board (sel1 pos+1, sel2 pos +1) == col && checkPositionOnBoard board (sel1 pos-1, sel2 pos -1) == True && checkPositionForPiece (sel1 pos-1, sel2 pos -1) board == True && getPositionColor board (sel1 pos-1, sel2 pos -1) == col = True
                                            |otherwise = False

checkMiddleDiagonalTwo :: Board -> Position -> Col -> Int -> Bool
checkMiddleDiagonalTwo board pos col target |target == 3 && checkPositionOnBoard board (sel1 pos+2, sel2 pos -2) == True && checkPositionForPiece (sel1 pos+2, sel2 pos -2) board == True && getPositionColor board (sel1 pos+2, sel2 pos -2) == col && checkPositionOnBoard board (sel1 pos+1, sel2 pos -1) == True && checkPositionForPiece (sel1 pos+1, sel2 pos -1) board == True && getPositionColor board (sel1 pos+1, sel2 pos -1) == col && checkPositionOnBoard board (sel1 pos-1, sel2 pos +1) == True && checkPositionForPiece (sel1 pos-1, sel2 pos +1) board == True && getPositionColor board (sel1 pos-1, sel2 pos +1) == col = False
                                            |target == 3 && checkPositionOnBoard board (sel1 pos-2, sel2 pos +2) == True && checkPositionForPiece (sel1 pos-2, sel2 pos +2) board == True && getPositionColor board (sel1 pos-2, sel2 pos +2) == col && checkPositionOnBoard board (sel1 pos+1, sel2 pos -1) == True && checkPositionForPiece (sel1 pos+1, sel2 pos -1) board == True && getPositionColor board (sel1 pos+1, sel2 pos -1) == col && checkPositionOnBoard board (sel1 pos-1, sel2 pos +1) == True && checkPositionForPiece (sel1 pos-1, sel2 pos +1) board == True && getPositionColor board (sel1 pos-1, sel2 pos +1) == col = False
                                            |target == 3 && checkPositionOnBoard board (sel1 pos+1, sel2 pos -1) == True && checkPositionForPiece (sel1 pos+1, sel2 pos -1) board == True && getPositionColor board (sel1 pos+1, sel2 pos -1) == col && checkPositionOnBoard board (sel1 pos-1, sel2 pos +1) == True && checkPositionForPiece (sel1 pos-1, sel2 pos +1) board == True && getPositionColor board (sel1 pos-1, sel2 pos +1) == col = True
                                            |otherwise = False

-- An evaluation function for a minimax search. Given a board and a colour
-- return an integer indicating how good the board is for that colour.

--let fil x = checkPositionForPiece x board == False in filter fil possiblePositions

checkAbove :: Position -> Col -> Board -> Bool
checkAbove pos col board | checkPositionOnBoard board (sel1 pos-1, sel2 pos) == True && checkPositionForPiece (sel1 pos-1, sel2 pos) board == True && getPositionColor board (sel1 pos-1, sel2 pos) == col = True
                   |otherwise = False

checkBelow :: Position -> Col -> Board ->  Bool
checkBelow pos col board | checkPositionOnBoard board (sel1 pos+1, sel2 pos) == True && checkPositionForPiece (sel1 pos+1, sel2 pos) board == True && getPositionColor board (sel1 pos+1, sel2 pos) == col = True
                   |otherwise = False

checkLeft :: Position -> Col ->Board ->  Bool
checkLeft pos col board | checkPositionOnBoard board (sel1 pos, sel2 pos-1) == True && checkPositionForPiece (sel1 pos, sel2 pos-1) board == True && getPositionColor board (sel1 pos, sel2 pos-1) == col = True
                  |otherwise = False

checkRight :: Position -> Col -> Board ->  Bool
checkRight pos col board | checkPositionOnBoard board (sel1 pos, sel2 pos+1) == True && checkPositionForPiece (sel1 pos, sel2 pos+1) board == True && getPositionColor board (sel1 pos, sel2 pos+1) == col = True
                   |otherwise = False

checkUpperLeft :: Position -> Col -> Board ->  Bool
checkUpperLeft pos col board | checkPositionOnBoard board (sel1 pos-1, sel2 pos-1) == True && checkPositionForPiece (sel1 pos-1, sel2 pos-1) board == True && getPositionColor board (sel1 pos-1, sel2 pos-1) == col = True
                  |otherwise = False

checkUpperRight:: Position -> Col -> Board ->  Bool
checkUpperRight pos col board | checkPositionOnBoard board (sel1 pos-1, sel2 pos+1) == True && checkPositionForPiece (sel1 pos-1, sel2 pos+1) board == True && getPositionColor board (sel1 pos-1, sel2 pos+1) == col = True
                   |otherwise = False

checkLowerLeft :: Position -> Col -> Board ->  Bool
checkLowerLeft pos col board | checkPositionOnBoard board (sel1 pos+1, sel2 pos-1) == True && checkPositionForPiece (sel1 pos+1, sel2 pos-1) board == True && getPositionColor board (sel1 pos+1, sel2 pos-1) == col = True
                  |otherwise = False

checkLowerRight :: Position -> Col -> Board ->  Bool
checkLowerRight pos col board | checkPositionOnBoard board (sel1 pos+1, sel2 pos+1) == True && checkPositionForPiece (sel1 pos+1, sel2 pos+1) board == True && getPositionColor board (sel1 pos+1, sel2 pos+1) == col = True
                 |otherwise = False

checkWhatColor :: (Position, Col) -> Board -> Col -> Bool
checkWhatColor pos board col | getPositionColor board (sel1 pos) == col = True
                             |otherwise = False

checkForTwoInARow :: [(Position, Col)] -> Col -> Board -> Bool
checkForTwoInARow pieces col board | length (let fil x = checkAbove (sel1 x) col board == True || checkBelow (sel1 x) col board == True || checkRight (sel1 x) col board == True || checkLeft (sel1 x) col board == True|| checkUpperLeft (sel1 x) col board == True|| checkLowerLeft (sel1 x) col board == True|| checkUpperRight (sel1 x) col board == True|| checkLowerRight (sel1 x) col board == True in filter fil pieces) == (length pieces) = True
                             |otherwise = False

checkForThreeInARow :: [(Position, Col)] -> Col -> Board -> Bool
checkForThreeInARow pieces col board | length (let fil x = checkEast board (sel1 x) col (target board) == True || checkWest board (sel1 x) col (target board) == True || checkMiddleRow board (sel1 x) col (target board) == True || checkNorth board (sel1 x) col (target board) == True|| checkSouth board (sel1 x) col (target board) == True|| checkMiddleColumn board (sel1 x) col (target board) == True|| checkNorthEast board (sel1 x) col (target board) == True|| checkSouthWest board (sel1 x) col (target board) == True || checkNorthWest board (sel1 x) col (target board) == True || checkSouthEast board (sel1 x) col (target board) == True || checkMiddleDiagonalOne board (sel1 x) col (target board) == True || checkMiddleDiagonalTwo board (sel1 x) col (target board) == True in filter fil pieces) > 0 = True
                            |otherwise = False


checkForDefence :: (Position, Col) -> Col -> Board -> Bool
checkForDefence piece col board |checkEast board (sel1 piece) (other col) (target board) == True || checkWest board (sel1 piece) (other col) (target board) == True || checkMiddleRow board (sel1 piece) (other col) (target board) == True || checkNorth board (sel1 piece) (other col) (target board) == True|| checkSouth board (sel1 piece) (other col) (target board) == True|| checkMiddleColumn board (sel1 piece) (other col) (target board) == True|| checkNorthEast board (sel1 piece) (other col) (target board) == True|| checkSouthWest board (sel1 piece) (other col) (target board) == True || checkNorthWest board (sel1 piece) (other col) (target board) == True || checkSouthEast board (sel1 piece) (other col) (target board) == True || checkMiddleDiagonalOne board (sel1 piece) (other col) (target board) == True || checkMiddleDiagonalTwo board (sel1 piece) (other col) (target board) == True = True
                                |otherwise = False


getPiecesForColor :: Board -> Col -> [(Position, Col)]
getPiecesForColor board col = let fil x = checkWhatColor x board col == True in filter fil (pieces board)

--if there are 3 in a row then return 3
--if there are 2 in a row then return 2
--otherwise return 1
evaluate :: Board -> Col -> Int
evaluate board col | checkForThreeInARow (getPiecesForColor board col) col board == True = 5
                   | checkForDefence ((pieces board) !! 0) col board == True = 4
                   | checkForTwoInARow (getPiecesForColor board col) col board == True = 2
                   | otherwise = 1
