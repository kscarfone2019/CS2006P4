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

checkPositionOnBoard :: Board -> Position -> Bool
checkPositionOnBoard board pos = elem pos (map (\ posi -> sel2  posi) piecesOnBoard)
		       where piecesOnBoard = positions

checkPositionForPiece :: Position -> Board -> Bool
checkPositionForPiece pos board = elem pos (map (\ posi -> sel1  posi) piecesOnBoard)
		          where piecesOnBoard = pieces board

positions = [("OneOne", (1,1)),
	     ("OneTwo", (1,2)),
	     ("OneThree", (1,3)),
	     ("OneFour", (1,4)),
	     ("OneFive", (1,5)),
	     ("OneSix", (1,6)),
	     ("TwoOne", (2,1)),
	     ("TwoTwo", (2,2)),
	     ("TwoThree", (2,3)),
	     ("TwoFour", (2,4)),
	     ("TwoFive", (2,5)),
	     ("TwoSix", (2,6)),
	     ("ThreeOne", (3,1)),
	     ("ThreeTwo", (3,2)),
	     ("ThreeThree", (3,3)),
	     ("ThreeFour", (3,4)),
	     ("ThreeFive", (3,5)),
	     ("ThreeSix", (3,6)),
	     ("FourOne", (4,1)),
	     ("FourTwo", (4,2)),
	     ("FourThree", (4,3)),
	     ("FourFour", (4,4)),
	     ("FourFive", (4,5)),
	     ("FourSix", (4,6)),
	     ("FiveOne", (5,1)),
	     ("FiveTwo", (5,2)),
	     ("FiveThree", (5,3)),
	     ("FiveFour", (5,4)),
	     ("FiveFive", (5,5)),
	     ("FiveSix", (5,6)),
	     ("SixOne", (6,1)),
	     ("SixTwo", (6,2)),
	     ("SixThree", (6,3)),
	     ("SixFour", (6,4)),
	     ("SixFive", (6,5)),
	     ("SixSix", (6,6))]

getPositionColor :: Board -> Position -> Col
getPositionColor board pos | checkPositionOnBoard board pos == True && checkPositionForPiece pos board == True = sel2 (head[x | x <- pieces board, sel1 x == pos])
			                     | otherwise = Empty


-- Check whether the board is in a winning state for either player.
-- Returns 'Nothing' if neither player has won yet
-- Returns 'Just c' if the player 'c' has won

checkWon :: Board -> Maybe Col
checkWon board |checkEast board (sel1 (head (pieces board))) (getPositionColor board (sel1 (head (pieces board)))) == True = Just (getPositionColor board (sel1 (head (pieces board))))
               |checkWest board (sel1 (head (pieces board))) (getPositionColor board (sel1 (head (pieces board)))) == True = Just (getPositionColor board (sel1 (head (pieces board))))
               |checkMiddleRow board (sel1 (head (pieces board))) (getPositionColor board (sel1 (head (pieces board)))) == True = Just (getPositionColor board (sel1 (head (pieces board))))
	             |checkNorth board (sel1 (head (pieces board))) (getPositionColor board (sel1 (head (pieces board)))) == True = Just (getPositionColor board (sel1 (head (pieces board))))
               |checkSouth board (sel1 (head (pieces board))) (getPositionColor board (sel1 (head (pieces board)))) == True = Just (getPositionColor board (sel1 (head (pieces board))))
               |checkMiddleColumn board (sel1 (head (pieces board))) (getPositionColor board (sel1 (head (pieces board)))) == True = Just (getPositionColor board (sel1 (head (pieces board))))
	             |checkNorthEast board (sel1 (head (pieces board))) (getPositionColor board (sel1 (head (pieces board)))) == True = Just (getPositionColor board (sel1 (head (pieces board))))
               |checkSouthWest board (sel1 (head (pieces board))) (getPositionColor board (sel1 (head (pieces board)))) == True = Just (getPositionColor board (sel1 (head (pieces board))))
               |checkNorthWest board (sel1 (head (pieces board))) (getPositionColor board (sel1 (head (pieces board)))) == True = Just (getPositionColor board (sel1 (head (pieces board))))
               |checkSouthEast board (sel1 (head (pieces board))) (getPositionColor board (sel1 (head (pieces board)))) == True = Just (getPositionColor board (sel1 (head (pieces board))))
               |checkMiddleDiagonalOne board (sel1 (head (pieces board))) (getPositionColor board (sel1 (head (pieces board)))) == True = Just (getPositionColor board (sel1 (head (pieces board))))
               |checkMiddleDiagonalTwo board (sel1 (head (pieces board))) (getPositionColor board (sel1 (head (pieces board)))) == True = Just (getPositionColor board (sel1 (head (pieces board))))
	             |otherwise = Nothing

checkEast :: Board -> Position -> Col -> Bool
checkEast board pos col |checkPositionOnBoard board (sel1 pos -1, sel2 pos) == True && checkPositionForPiece (sel1 pos -1, sel2 pos) board == True && getPositionColor board (sel1 pos -1, sel2 pos) == col && checkPositionOnBoard board (sel1 pos +1, sel2 pos) == True && checkPositionForPiece (sel1 pos +1, sel2 pos) board == True && getPositionColor board (sel1 pos +1, sel2 pos) == col && checkPositionOnBoard board (sel1 pos +2, sel2 pos) == True && checkPositionForPiece (sel1 pos +2, sel2 pos) board == True && getPositionColor board (sel1 pos +2, sel2 pos) == col = False
                        |checkPositionOnBoard board (sel1 pos +3, sel2 pos) == True && checkPositionForPiece (sel1 pos +3, sel2 pos) board == True && getPositionColor board (sel1 pos +3, sel2 pos) == col && checkPositionOnBoard board (sel1 pos +1, sel2 pos) == True && checkPositionForPiece (sel1 pos +1, sel2 pos) board == True && getPositionColor board (sel1 pos +1, sel2 pos) == col && checkPositionOnBoard board (sel1 pos +2, sel2 pos) == True && checkPositionForPiece (sel1 pos +2, sel2 pos) board == True && getPositionColor board (sel1 pos +2, sel2 pos) == col = False
                        |checkPositionOnBoard board (sel1 pos +1, sel2 pos) == True && checkPositionForPiece (sel1 pos +1, sel2 pos) board == True && getPositionColor board (sel1 pos +1, sel2 pos) == col && checkPositionOnBoard board (sel1 pos +2, sel2 pos) == True && checkPositionForPiece (sel1 pos +2, sel2 pos) board == True && getPositionColor board (sel1 pos +2, sel2 pos) == col = True
                        |otherwise = False

checkWest :: Board -> Position -> Col -> Bool
checkWest board pos col |checkPositionOnBoard board (sel1 pos -3, sel2 pos) == True && checkPositionForPiece (sel1 pos -3, sel2 pos) board == True && getPositionColor board (sel1 pos -3, sel2 pos) == col && checkPositionOnBoard board (sel1 pos -2, sel2 pos) == True && checkPositionForPiece (sel1 pos -2, sel2 pos) board == True && getPositionColor board (sel1 pos -2, sel2 pos) == col && checkPositionOnBoard board (sel1 pos -1, sel2 pos) == True && checkPositionForPiece (sel1 pos -1, sel2 pos) board == True && getPositionColor board (sel1 pos -1, sel2 pos) == col = False
                        |checkPositionOnBoard board (sel1 pos +1, sel2 pos) == True && checkPositionForPiece (sel1 pos +1, sel2 pos) board == True && getPositionColor board (sel1 pos +1, sel2 pos) == col && checkPositionOnBoard board (sel1 pos -2, sel2 pos) == True && checkPositionForPiece (sel1 pos -2, sel2 pos) board == True && getPositionColor board (sel1 pos -2, sel2 pos) == col && checkPositionOnBoard board (sel1 pos -1, sel2 pos) == True && checkPositionForPiece (sel1 pos -1, sel2 pos) board == True && getPositionColor board (sel1 pos -1, sel2 pos) == col = False
                        |checkPositionOnBoard board (sel1 pos -2, sel2 pos) == True && checkPositionForPiece (sel1 pos -2, sel2 pos) board == True && getPositionColor board (sel1 pos -2, sel2 pos) == col && checkPositionOnBoard board (sel1 pos -1, sel2 pos) == True && checkPositionForPiece (sel1 pos -1, sel2 pos) board == True && getPositionColor board (sel1 pos -1, sel2 pos) == col = True
                        |otherwise = False

checkMiddleRow :: Board -> Position -> Col -> Bool
checkMiddleRow board pos col |checkPositionOnBoard board (sel1 pos -2, sel2 pos) == True && checkPositionForPiece (sel1 pos -2, sel2 pos) board == True && getPositionColor board (sel1 pos -2, sel2 pos) == col && checkPositionOnBoard board (sel1 pos -1, sel2 pos) == True && checkPositionForPiece (sel1 pos -1, sel2 pos) board == True && getPositionColor board (sel1 pos -1, sel2 pos) == col && checkPositionOnBoard board (sel1 pos +1, sel2 pos) == True && checkPositionForPiece (sel1 pos +1, sel2 pos) board == True && getPositionColor board (sel1 pos +1, sel2 pos) == col = False
                             |checkPositionOnBoard board (sel1 pos +2, sel2 pos) == True && checkPositionForPiece (sel1 pos +2, sel2 pos) board == True && getPositionColor board (sel1 pos +2, sel2 pos) == col && checkPositionOnBoard board (sel1 pos -1, sel2 pos) == True && checkPositionForPiece (sel1 pos -1, sel2 pos) board == True && getPositionColor board (sel1 pos -1, sel2 pos) == col && checkPositionOnBoard board (sel1 pos +1, sel2 pos) == True && checkPositionForPiece (sel1 pos +1, sel2 pos) board == True && getPositionColor board (sel1 pos +1, sel2 pos) == col = False
                             |checkPositionOnBoard board (sel1 pos -1, sel2 pos) == True && checkPositionForPiece (sel1 pos -1, sel2 pos) board == True && getPositionColor board (sel1 pos -1, sel2 pos) == col && checkPositionOnBoard board (sel1 pos +1, sel2 pos) == True && checkPositionForPiece (sel1 pos +1, sel2 pos) board == True && getPositionColor board (sel1 pos +1, sel2 pos) == col = True
                             |otherwise = False

checkNorth :: Board -> Position -> Col -> Bool
checkNorth board pos col |checkPositionOnBoard board (sel1 pos, sel2 pos -3) == True && checkPositionForPiece (sel1 pos, sel2 pos -3) board == True && getPositionColor board (sel1 pos, sel2 pos -3) == col && checkPositionOnBoard board (sel1 pos, sel2 pos -2) == True && checkPositionForPiece (sel1 pos, sel2 pos -2) board == True && getPositionColor board (sel1 pos, sel2 pos -2) == col && checkPositionOnBoard board (sel1 pos, sel2 pos -1) == True && checkPositionForPiece (sel1 pos, sel2 pos -1) board == True && getPositionColor board (sel1 pos, sel2 pos -1) == col = False
                         |checkPositionOnBoard board (sel1 pos, sel2 pos +1) == True && checkPositionForPiece (sel1 pos, sel2 pos +1) board == True && getPositionColor board (sel1 pos, sel2 pos +1) == col && checkPositionOnBoard board (sel1 pos, sel2 pos -2) == True && checkPositionForPiece (sel1 pos, sel2 pos -2) board == True && getPositionColor board (sel1 pos, sel2 pos -2) == col && checkPositionOnBoard board (sel1 pos, sel2 pos -1) == True && checkPositionForPiece (sel1 pos, sel2 pos -1) board == True && getPositionColor board (sel1 pos, sel2 pos -1) == col = False
                         |checkPositionOnBoard board (sel1 pos, sel2 pos -2) == True && checkPositionForPiece (sel1 pos, sel2 pos -2) board == True && getPositionColor board (sel1 pos, sel2 pos -2) == col && checkPositionOnBoard board (sel1 pos, sel2 pos -1) == True && checkPositionForPiece (sel1 pos, sel2 pos -1) board == True && getPositionColor board (sel1 pos, sel2 pos -1) == col = True
                         |otherwise = False

checkSouth :: Board -> Position -> Col -> Bool
checkSouth board pos col |checkPositionOnBoard board (sel1 pos, sel2 pos +3) == True && checkPositionForPiece (sel1 pos, sel2 pos +3) board == True && getPositionColor board (sel1 pos, sel2 pos +3) == col && checkPositionOnBoard board (sel1 pos, sel2 pos +1) == True && checkPositionForPiece (sel1 pos, sel2 pos +1) board == True && getPositionColor board (sel1 pos, sel2 pos +1) == col && checkPositionOnBoard board (sel1 pos, sel2 pos +2) == True && checkPositionForPiece (sel1 pos, sel2 pos +2) board == True && getPositionColor board (sel1 pos, sel2 pos +2) == col = False
                         |checkPositionOnBoard board (sel1 pos, sel2 pos -1) == True && checkPositionForPiece (sel1 pos, sel2 pos -1) board == True && getPositionColor board (sel1 pos, sel2 pos -1) == col && checkPositionOnBoard board (sel1 pos, sel2 pos +1) == True && checkPositionForPiece (sel1 pos, sel2 pos +1) board == True && getPositionColor board (sel1 pos, sel2 pos +1) == col && checkPositionOnBoard board (sel1 pos, sel2 pos +2) == True && checkPositionForPiece (sel1 pos, sel2 pos +2) board == True && getPositionColor board (sel1 pos, sel2 pos +2) == col = False
                         |checkPositionOnBoard board (sel1 pos, sel2 pos +1) == True && checkPositionForPiece (sel1 pos, sel2 pos +1) board == True && getPositionColor board (sel1 pos, sel2 pos +1) == col && checkPositionOnBoard board (sel1 pos, sel2 pos +2) == True && checkPositionForPiece (sel1 pos, sel2 pos +2) board == True && getPositionColor board (sel1 pos, sel2 pos +2) == col = True
                         |otherwise = False

checkMiddleColumn :: Board -> Position -> Col -> Bool
checkMiddleColumn board pos col |checkPositionOnBoard board (sel1 pos, sel2 pos -2) == True && checkPositionForPiece (sel1 pos, sel2 pos -2) board == True && getPositionColor board (sel1 pos, sel2 pos -2) == col && checkPositionOnBoard board (sel1 pos, sel2 pos -1) == True && checkPositionForPiece (sel1 pos, sel2 pos -1) board == True && getPositionColor board (sel1 pos, sel2 pos -1) == col && checkPositionOnBoard board (sel1 pos, sel2 pos +1) == True && checkPositionForPiece (sel1 pos, sel2 pos +1) board == True && getPositionColor board (sel1 pos, sel2 pos +1) == col = False
                                |checkPositionOnBoard board (sel1 pos, sel2 pos +2) == True && checkPositionForPiece (sel1 pos, sel2 pos +2) board == True && getPositionColor board (sel1 pos, sel2 pos +2) == col && checkPositionOnBoard board (sel1 pos, sel2 pos -1) == True && checkPositionForPiece (sel1 pos, sel2 pos -1) board == True && getPositionColor board (sel1 pos, sel2 pos -1) == col && checkPositionOnBoard board (sel1 pos, sel2 pos +1) == True && checkPositionForPiece (sel1 pos, sel2 pos +1) board == True && getPositionColor board (sel1 pos, sel2 pos +1) == col = False
                                |checkPositionOnBoard board (sel1 pos, sel2 pos -1) == True && checkPositionForPiece (sel1 pos, sel2 pos -1) board == True && getPositionColor board (sel1 pos, sel2 pos -1) == col && checkPositionOnBoard board (sel1 pos, sel2 pos +1) == True && checkPositionForPiece (sel1 pos, sel2 pos +1) board == True && getPositionColor board (sel1 pos, sel2 pos +1) == col = True
                                |otherwise = False

checkNorthEast :: Board -> Position -> Col -> Bool
checkNorthEast board pos col |checkPositionOnBoard board (sel1 pos+3, sel2 pos -3) == True && checkPositionForPiece (sel1 pos+3, sel2 pos -3) board == True && getPositionColor board (sel1 pos+3, sel2 pos -3) == col && checkPositionOnBoard board (sel1 pos+1, sel2 pos -1) == True && checkPositionForPiece (sel1 pos+1, sel2 pos -1) board == True && getPositionColor board (sel1 pos+1, sel2 pos -1) == col && checkPositionOnBoard board (sel1 pos+2, sel2 pos -2) == True && checkPositionForPiece (sel1 pos+2, sel2 pos -2) board == True && getPositionColor board (sel1 pos+2, sel2 pos -2) == col = False
                             |checkPositionOnBoard board (sel1 pos-1, sel2 pos +1) == True && checkPositionForPiece (sel1 pos-1, sel2 pos +1) board == True && getPositionColor board (sel1 pos-1, sel2 pos +1) == col && checkPositionOnBoard board (sel1 pos+1, sel2 pos -1) == True && checkPositionForPiece (sel1 pos+1, sel2 pos -1) board == True && getPositionColor board (sel1 pos+1, sel2 pos -1) == col && checkPositionOnBoard board (sel1 pos+2, sel2 pos -2) == True && checkPositionForPiece (sel1 pos+2, sel2 pos -2) board == True && getPositionColor board (sel1 pos+2, sel2 pos -2) == col = False
                             |checkPositionOnBoard board (sel1 pos+1, sel2 pos -1) == True && checkPositionForPiece (sel1 pos+1, sel2 pos -1) board == True && getPositionColor board (sel1 pos+1, sel2 pos -1) == col && checkPositionOnBoard board (sel1 pos+2, sel2 pos -2) == True && checkPositionForPiece (sel1 pos+2, sel2 pos -2) board == True && getPositionColor board (sel1 pos+2, sel2 pos -2) == col = True
                             |otherwise = False

checkSouthWest :: Board -> Position -> Col -> Bool
checkSouthWest board pos col |checkPositionOnBoard board (sel1 pos-3, sel2 pos +3) == True && checkPositionForPiece (sel1 pos-3, sel2 pos +3) board == True && getPositionColor board (sel1 pos-3, sel2 pos +3) == col && checkPositionOnBoard board (sel1 pos-1, sel2 pos +1) == True && checkPositionForPiece (sel1 pos-1, sel2 pos +1) board == True && getPositionColor board (sel1 pos-1, sel2 pos +1) == col && checkPositionOnBoard board (sel1 pos-2, sel2 pos +2) == True && checkPositionForPiece (sel1 pos-2, sel2 pos +2) board == True && getPositionColor board (sel1 pos-2, sel2 pos +2) == col = False
                             |checkPositionOnBoard board (sel1 pos+1, sel2 pos -1) == True && checkPositionForPiece (sel1 pos+1, sel2 pos -1) board == True && getPositionColor board (sel1 pos+1, sel2 pos -1) == col && checkPositionOnBoard board (sel1 pos-1, sel2 pos +1) == True && checkPositionForPiece (sel1 pos-1, sel2 pos +1) board == True && getPositionColor board (sel1 pos-1, sel2 pos +1) == col && checkPositionOnBoard board (sel1 pos-2, sel2 pos +2) == True && checkPositionForPiece (sel1 pos-2, sel2 pos +2) board == True && getPositionColor board (sel1 pos-2, sel2 pos +2) == col = False
                             |checkPositionOnBoard board (sel1 pos-1, sel2 pos +1) == True && checkPositionForPiece (sel1 pos-1, sel2 pos +1) board == True && getPositionColor board (sel1 pos-1, sel2 pos +1) == col && checkPositionOnBoard board (sel1 pos-2, sel2 pos +2) == True && checkPositionForPiece (sel1 pos-2, sel2 pos +2) board == True && getPositionColor board (sel1 pos-2, sel2 pos +2) == col = True
                             |otherwise = False

checkNorthWest :: Board -> Position -> Col -> Bool
checkNorthWest board pos col |checkPositionOnBoard board (sel1 pos-3, sel2 pos -3) == True && checkPositionForPiece (sel1 pos-3, sel2 pos -3) board == True && getPositionColor board (sel1 pos-3, sel2 pos -3) == col && checkPositionOnBoard board (sel1 pos-1, sel2 pos -1) == True && checkPositionForPiece (sel1 pos-1, sel2 pos -1) board == True && getPositionColor board (sel1 pos-1, sel2 pos -1) == col && checkPositionOnBoard board (sel1 pos-2, sel2 pos -2) == True && checkPositionForPiece (sel1 pos-2, sel2 pos -2) board == True && getPositionColor board (sel1 pos-2, sel2 pos -2) == col = False
                             |checkPositionOnBoard board (sel1 pos+1, sel2 pos +1) == True && checkPositionForPiece (sel1 pos+1, sel2 pos +1) board == True && getPositionColor board (sel1 pos+1, sel2 pos +1) == col && checkPositionOnBoard board (sel1 pos-1, sel2 pos -1) == True && checkPositionForPiece (sel1 pos-1, sel2 pos -1) board == True && getPositionColor board (sel1 pos-1, sel2 pos -1) == col && checkPositionOnBoard board (sel1 pos-2, sel2 pos -2) == True && checkPositionForPiece (sel1 pos-2, sel2 pos -2) board == True && getPositionColor board (sel1 pos-2, sel2 pos -2) == col = False
                             |checkPositionOnBoard board (sel1 pos-1, sel2 pos -1) == True && checkPositionForPiece (sel1 pos-1, sel2 pos -1) board == True && getPositionColor board (sel1 pos-1, sel2 pos -1) == col && checkPositionOnBoard board (sel1 pos-2, sel2 pos -2) == True && checkPositionForPiece (sel1 pos-2, sel2 pos -2) board == True && getPositionColor board (sel1 pos-2, sel2 pos -2) == col = True
		                         |otherwise = False

checkSouthEast :: Board -> Position -> Col -> Bool
checkSouthEast board pos col |checkPositionOnBoard board (sel1 pos+3, sel2 pos +3) == True && checkPositionForPiece (sel1 pos+3, sel2 pos +3) board == True && getPositionColor board (sel1 pos+3, sel2 pos +3) == col && checkPositionOnBoard board (sel1 pos+1, sel2 pos +1) == True && checkPositionForPiece (sel1 pos+1, sel2 pos +1) board == True && getPositionColor board (sel1 pos+1, sel2 pos +1) == col && checkPositionOnBoard board (sel1 pos+2, sel2 pos +2) == True && checkPositionForPiece (sel1 pos+2, sel2 pos +2) board == True && getPositionColor board (sel1 pos+2, sel2 pos +2) == col = False
                             |checkPositionOnBoard board (sel1 pos-1, sel2 pos -1) == True && checkPositionForPiece (sel1 pos-1, sel2 pos -1) board == True && getPositionColor board (sel1 pos-1, sel2 pos -1) == col && checkPositionOnBoard board (sel1 pos+1, sel2 pos +1) == True && checkPositionForPiece (sel1 pos+1, sel2 pos +1) board == True && getPositionColor board (sel1 pos+1, sel2 pos +1) == col && checkPositionOnBoard board (sel1 pos+2, sel2 pos +2) == True && checkPositionForPiece (sel1 pos+2, sel2 pos +2) board == True && getPositionColor board (sel1 pos+2, sel2 pos +2) == col = False
                             |checkPositionOnBoard board (sel1 pos+1, sel2 pos +1) == True && checkPositionForPiece (sel1 pos+1, sel2 pos +1) board == True && getPositionColor board (sel1 pos+1, sel2 pos +1) == col && checkPositionOnBoard board (sel1 pos+2, sel2 pos +2) == True && checkPositionForPiece (sel1 pos+2, sel2 pos +2) board == True && getPositionColor board (sel1 pos+2, sel2 pos +2) == col = True
                             |otherwise = False

checkMiddleDiagonalOne :: Board -> Position -> Col -> Bool
checkMiddleDiagonalOne board pos col |checkPositionOnBoard board (sel1 pos+2, sel2 pos +2) == True && checkPositionForPiece (sel1 pos+2, sel2 pos +2) board == True && getPositionColor board (sel1 pos+2, sel2 pos +2) == col && checkPositionOnBoard board (sel1 pos+1, sel2 pos +1) == True && checkPositionForPiece (sel1 pos+1, sel2 pos +1) board == True && getPositionColor board (sel1 pos+1, sel2 pos +1) == col && checkPositionOnBoard board (sel1 pos-1, sel2 pos -1) == True && checkPositionForPiece (sel1 pos-1, sel2 pos -1) board == True && getPositionColor board (sel1 pos-1, sel2 pos -1) == col = False
                                     |checkPositionOnBoard board (sel1 pos-2, sel2 pos -2) == True && checkPositionForPiece (sel1 pos-2, sel2 pos -2) board == True && getPositionColor board (sel1 pos-2, sel2 pos -2) == col && checkPositionOnBoard board (sel1 pos+1, sel2 pos +1) == True && checkPositionForPiece (sel1 pos+1, sel2 pos +1) board == True && getPositionColor board (sel1 pos+1, sel2 pos +1) == col && checkPositionOnBoard board (sel1 pos-1, sel2 pos -1) == True && checkPositionForPiece (sel1 pos-1, sel2 pos -1) board == True && getPositionColor board (sel1 pos-1, sel2 pos -1) == col = False
                                     |checkPositionOnBoard board (sel1 pos+1, sel2 pos +1) == True && checkPositionForPiece (sel1 pos+1, sel2 pos +1) board == True && getPositionColor board (sel1 pos+1, sel2 pos +1) == col && checkPositionOnBoard board (sel1 pos-1, sel2 pos -1) == True && checkPositionForPiece (sel1 pos-1, sel2 pos -1) board == True && getPositionColor board (sel1 pos-1, sel2 pos -1) == col = True
                                     |otherwise = False

checkMiddleDiagonalTwo :: Board -> Position -> Col -> Bool
checkMiddleDiagonalTwo board pos col |checkPositionOnBoard board (sel1 pos+2, sel2 pos -2) == True && checkPositionForPiece (sel1 pos+2, sel2 pos -2) board == True && getPositionColor board (sel1 pos+2, sel2 pos -2) == col && checkPositionOnBoard board (sel1 pos+1, sel2 pos -1) == True && checkPositionForPiece (sel1 pos+1, sel2 pos -1) board == True && getPositionColor board (sel1 pos+1, sel2 pos -1) == col && checkPositionOnBoard board (sel1 pos-1, sel2 pos +1) == True && checkPositionForPiece (sel1 pos-1, sel2 pos +1) board == True && getPositionColor board (sel1 pos-1, sel2 pos +1) == col = False
                                     |checkPositionOnBoard board (sel1 pos-2, sel2 pos +2) == True && checkPositionForPiece (sel1 pos-2, sel2 pos +2) board == True && getPositionColor board (sel1 pos-2, sel2 pos +2) == col && checkPositionOnBoard board (sel1 pos+1, sel2 pos -1) == True && checkPositionForPiece (sel1 pos+1, sel2 pos -1) board == True && getPositionColor board (sel1 pos+1, sel2 pos -1) == col && checkPositionOnBoard board (sel1 pos-1, sel2 pos +1) == True && checkPositionForPiece (sel1 pos-1, sel2 pos +1) board == True && getPositionColor board (sel1 pos-1, sel2 pos +1) == col = False
                                     |checkPositionOnBoard board (sel1 pos+1, sel2 pos -1) == True && checkPositionForPiece (sel1 pos+1, sel2 pos -1) board == True && getPositionColor board (sel1 pos+1, sel2 pos -1) == col && checkPositionOnBoard board (sel1 pos-1, sel2 pos +1) == True && checkPositionForPiece (sel1 pos-1, sel2 pos +1) board == True && getPositionColor board (sel1 pos-1, sel2 pos +1) == col = True
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
checkForThreeInARow pieces col board | length (let fil x = checkEast board (sel1 x) col == True || checkWest board (sel1 x) col == True || checkMiddleRow board (sel1 x) col == True || checkNorth board (sel1 x) col == True|| checkSouth board (sel1 x) col == True|| checkMiddleColumn board (sel1 x) col == True|| checkNorthEast board (sel1 x) col == True|| checkSouthWest board (sel1 x) col == True || checkNorthWest board (sel1 x) col == True || checkSouthEast board (sel1 x) col == True || checkMiddleDiagonalOne board (sel1 x) col == True || checkMiddleDiagonalTwo board (sel1 x) col == True in filter fil pieces) > 0 = True
                            |otherwise = False


checkForDefence :: (Position, Col) -> Col -> Board -> Bool
checkForDefence piece col board |checkEast board (sel1 piece) (other col) == True || checkWest board (sel1 piece) (other col) == True || checkMiddleRow board (sel1 piece) (other col) == True || checkNorth board (sel1 piece) (other col) == True|| checkSouth board (sel1 piece) (other col) == True|| checkMiddleColumn board (sel1 piece) (other col) == True|| checkNorthEast board (sel1 piece) (other col) == True|| checkSouthWest board (sel1 piece) (other col) == True || checkNorthWest board (sel1 piece) (other col) == True || checkSouthEast board (sel1 piece) (other col) == True || checkMiddleDiagonalOne board (sel1 piece) (other col) == True || checkMiddleDiagonalTwo board (sel1 piece) (other col) == True = True
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
