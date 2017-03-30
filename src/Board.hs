module Board where

import Data.Tuple.Select

data Col = Black | White
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
                     turn :: Col }

initWorld = World initBoard Black

-- Play a move on the board; return 'Nothing' if the move is invalid
-- (e.g. outside the range of the board, or there is a piece already there)
makeMove :: Board -> Col -> Position -> Maybe Board
makeMove board col pos |emptyPosition pos board == False && checkPositionOnBoard board pos == True = Just (Board (size board) (target board) ([(pos, col)]++(pieces board)))
		       |otherwise = Nothing

checkPositionOnBoard :: Board -> Position -> Bool
checkPositionOnBoard board pos = elem pos (map (\ posi -> sel2  posi) piecesOnBoard)
		       where piecesOnBoard = positions

emptyPosition :: Position -> Board -> Bool
emptyPosition pos board = elem pos (map (\ posi -> sel1  posi) piecesOnBoard)
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
getPositionColor board pos = sel2 (head[x | x <- pieces board, sel1 x == pos])
-- Check whether the board is in a winning state for either player.
-- Returns 'Nothing' if neither player has won yet
-- Returns 'Just c' if the player 'c' has won
checkWon :: Board -> Maybe Col
checkWon board = undefined

checkRows :: Board -> Position -> Bool
checkRows board pos |checkPositionOnBoard board (sel1 pos -2, sel2 pos) == True && emptyPosition (sel1 pos -2, sel2 pos) board == False && getPositionColor board (sel1 pos -2, sel2 pos) == Black && checkPositionOnBoard board (sel1 pos -1, sel2 pos) == True && emptyPosition (sel1 pos -1, sel2 pos) board == False && getPositionColor board (sel1 pos -1, sel2 pos) == Black && checkPositionOnBoard board pos == True && emptyPosition pos board == False && getPositionColor board pos == Black = True
                    |checkPositionOnBoard board (sel1 pos -2, sel2 pos) == True && emptyPosition (sel1 pos -2, sel2 pos) board == False && getPositionColor board (sel1 pos -2, sel2 pos) == White && checkPositionOnBoard board (sel1 pos -1, sel2 pos) == True && emptyPosition (sel1 pos -1, sel2 pos) board == False && getPositionColor board (sel1 pos -1, sel2 pos) == White && checkPositionOnBoard board pos == True && emptyPosition pos board == False && getPositionColor board pos == White = True
                    |checkPositionOnBoard board (sel1 pos -1, sel2 pos) == True && emptyPosition (sel1 pos -1, sel2 pos) board == False && getPositionColor board (sel1 pos -1, sel2 pos) == Black && checkPositionOnBoard board pos == True && emptyPosition pos board == False && getPositionColor board pos == Black && checkPositionOnBoard board (sel1 pos +1, sel2 pos) == True && emptyPosition (sel1 pos +1, sel2 pos) board == False && getPositionColor board (sel1 pos +1, sel2 pos) == Black = True
                    |checkPositionOnBoard board (sel1 pos -1, sel2 pos) == True && emptyPosition (sel1 pos -1, sel2 pos) board == False && getPositionColor board (sel1 pos -1, sel2 pos) == White && checkPositionOnBoard board pos == True && emptyPosition pos board == False && getPositionColor board pos == White && checkPositionOnBoard board (sel1 pos +1, sel2 pos) == True && emptyPosition (sel1 pos +1, sel2 pos) board == False && getPositionColor board (sel1 pos +1, sel2 pos) == White = True
                    |checkPositionOnBoard board pos == True && emptyPosition pos board == False && getPositionColor board pos == Black && checkPositionOnBoard board (sel1 pos +1, sel2 pos) == True && emptyPosition (sel1 pos +1, sel2 pos) board == False && getPositionColor board (sel1 pos +1, sel2 pos) == Black && checkPositionOnBoard board (sel1 pos +2, sel2 pos) == True && emptyPosition (sel1 pos +2, sel2 pos) board == False && getPositionColor board (sel1 pos +2, sel2 pos) == Black = True
                    |checkPositionOnBoard board pos == True && emptyPosition pos board == False && getPositionColor board pos == White && checkPositionOnBoard board (sel1 pos +1, sel2 pos) == True && emptyPosition (sel1 pos +1, sel2 pos) board == False && getPositionColor board (sel1 pos +1, sel2 pos) == White && checkPositionOnBoard board (sel1 pos +2, sel2 pos) == True && emptyPosition (sel1 pos +2, sel2 pos) board == False && getPositionColor board (sel1 pos +2, sel2 pos) == White = True
                    |otherwise = False



{- Hint: One way to implement 'checkWon' would be to write functions
which specifically check for lines in all 8 possible directions
(NW, N, NE, E, W, SE, SW)

In these functions:
To check for a line of n in a row in a direction D:
For every position ((x, y), col) in the 'pieces' list:
- if n == 1, the colour 'col' has won
- if n > 1, move one step in direction D, and check for a line of
  n-1 in a row.
-}

-- An evaluation function for a minimax search. Given a board and a colour
-- return an integer indicating how good the board is for that colour.
evaluate :: Board -> Col -> Int
evaluate = undefined
