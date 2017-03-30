module Draw(drawWorld) where

import Graphics.Gloss
import Board
import Data.Tuple.Select

-- Given a world state, return a Picture which will render the world state.
drawWorld :: World -> Picture

drawWorld w = Pictures [
		--column 1
		printPositionOneOne (board w),
		printPositionOneTwo (board w),
		printPositionOneThree (board w),
		printPositionOneFour (board w),
		printPositionOneFive (board w),
		printPositionOneSix (board w),

		--column 2
		printPositionTwoOne (board w),
		printPositionTwoTwo (board w),
		printPositionTwoThree (board w),
		printPositionTwoFour (board w),
		printPositionTwoFive (board w),
		printPositionTwoSix (board w),
		
		--column 3
		printPositionThreeOne (board w),
		printPositionThreeTwo (board w),
		printPositionThreeThree (board w),
		printPositionThreeFour (board w),
		printPositionThreeFive (board w),
		printPositionThreeSix (board w),

		--column 4
		printPositionFourOne (board w),
		printPositionFourTwo (board w),
		printPositionFourThree (board w),
		printPositionFourFour (board w),
		printPositionFourFive (board w),
		printPositionFourSix (board w),

		--column 5
		printPositionFiveOne (board w),
		printPositionFiveTwo (board w),
		printPositionFiveThree (board w),
		printPositionFiveFour (board w),
		printPositionFiveFive (board w),
		printPositionFiveSix (board w),

		--column 6
		printPositionSixOne (board w),
		printPositionSixTwo (board w),
		printPositionSixThree (board w),
		printPositionSixFour (board w),
		printPositionSixFive (board w),
		printPositionSixSix (board w)
                ]

--column 1
printPositionOneOne :: Board -> Picture
printPositionOneOne board |pieceHere (1,1) board == True && getColour (1,1) board == Black = Pictures[
											Color blue (Translate (-250) 250 (rectangleSolid 75 75)),
											Color black (Translate (-250) 250 (circleSolid 35))
											]
		       |pieceHere (1,1) board == True && getColour (1,1) board == White = Pictures[
											Color blue (Translate (-250) 250 (rectangleSolid 75 75)),
											Color white (Translate (-250) 250 (circleSolid 35))
											]
		       |otherwise = Color blue (Translate (-250) 250 (rectangleSolid 75 75))

printPositionOneTwo :: Board -> Picture
printPositionOneTwo board |pieceHere (1,2) board == True && getColour (1,2) board == Black = Pictures[
											Color blue (Translate (-250) 150 (rectangleSolid 75 75)),
											Color black (Translate (-250) 150 (circleSolid 35))
											]
		       |pieceHere (1,2) board == True && getColour (1,2) board == White = Pictures[
											Color blue (Translate (-250) 150 (rectangleSolid 75 75)),
											Color white (Translate (-250) 150 (circleSolid 35))
											]
		       |otherwise = Color blue (Translate (-250) 150 (rectangleSolid 75 75))

printPositionOneThree :: Board -> Picture
printPositionOneThree board |pieceHere (1,3) board == True && getColour (1,3) board == Black = Pictures[
											Color blue (Translate (-250) 50 (rectangleSolid 75 75)),
											Color black (Translate (-250) 50 (circleSolid 35))
											]
		       |pieceHere (1,3) board == True && getColour (1,3) board == White = Pictures[
											Color blue (Translate (-250) 50 (rectangleSolid 75 75)),
											Color white (Translate (-250) 50 (circleSolid 35))
											]
		       |otherwise = Color blue (Translate (-250) 50 (rectangleSolid 75 75))

printPositionOneFour :: Board -> Picture
printPositionOneFour board |pieceHere (1,4) board == True && getColour (1,4) board == Black = Pictures[
											Color blue (Translate (-250) (-50) (rectangleSolid 75 75)),
											Color black (Translate (-250) (-50) (circleSolid 35))
											]
		       |pieceHere (1,4) board == True && getColour (1,4) board == White = Pictures[
											Color blue (Translate (-250) (-50) (rectangleSolid 75 75)),
											Color white (Translate (-250) (-50) (circleSolid 35))
											]
		       |otherwise = Color blue (Translate (-250) (-50) (rectangleSolid 75 75))

printPositionOneFive :: Board -> Picture
printPositionOneFive board |pieceHere (1,5) board == True && getColour (1,5) board == Black = Pictures[
											Color blue (Translate (-250) (-150) (rectangleSolid 75 75)),
											Color black (Translate (-250) (-150) (circleSolid 35))
											]
		       |pieceHere (1,5) board == True && getColour (1,5) board == White = Pictures[
											Color blue (Translate (-250) (-150) (rectangleSolid 75 75)),
											Color white (Translate (-250) (-150) (circleSolid 35))
											]
		       |otherwise = Color blue (Translate (-250) (-150) (rectangleSolid 75 75))

printPositionOneSix :: Board -> Picture
printPositionOneSix board |pieceHere (1,6) board == True && getColour (1,6) board == Black = Pictures[
											Color blue (Translate (-250) (-250) (rectangleSolid 75 75)),
											Color black (Translate (-250) (-250) (circleSolid 35))
											]
		       |pieceHere (1,6) board == True && getColour (1,6) board == White = Pictures[
											Color blue (Translate (-250) (-250) (rectangleSolid 75 75)),
											Color white (Translate (-250) (-250) (circleSolid 35))
											]
		       |otherwise = Color blue (Translate (-250) (-250) (rectangleSolid 75 75))

--column 2

printPositionTwoOne :: Board -> Picture
printPositionTwoOne board |pieceHere (2,1) board == True && getColour (2,1) board == Black = Pictures[
											Color blue (Translate (-150) 250 (rectangleSolid 75 75)),
											Color black (Translate (-150) 250 (circleSolid 35))
											]
		       |pieceHere (2,1) board == True && getColour (2,1) board == White = Pictures[
											Color blue (Translate (-150) 250 (rectangleSolid 75 75)),
											Color white (Translate (-150) 250 (circleSolid 35))
											]
		       |otherwise = Color blue (Translate (-150) 250 (rectangleSolid 75 75))

printPositionTwoTwo :: Board -> Picture
printPositionTwoTwo board |pieceHere (2,2) board == True && getColour (2,2) board == Black = Pictures[
											Color blue (Translate (-150) 150 (rectangleSolid 75 75)),
											Color black (Translate (-150) 150 (circleSolid 35))
											]
		       |pieceHere (2,2) board == True && getColour (2,2) board == White = Pictures[
											Color blue (Translate (-150) 150 (rectangleSolid 75 75)),
											Color white (Translate (-150) 150 (circleSolid 35))
											]
		       |otherwise = Color blue (Translate (-150) 150 (rectangleSolid 75 75))

printPositionTwoThree :: Board -> Picture
printPositionTwoThree board |pieceHere (2,3) board == True && getColour (2,3) board == Black = Pictures[
											Color blue (Translate (-150) 50 (rectangleSolid 75 75)),
											Color black (Translate (-150) 50 (circleSolid 35))
											]
		       |pieceHere (2,3) board == True && getColour (2,3) board == White = Pictures[
											Color blue (Translate (-150) 50 (rectangleSolid 75 75)),
											Color white (Translate (-150) 50 (circleSolid 35))
											]
		       |otherwise = Color blue (Translate (-150) 50 (rectangleSolid 75 75))

printPositionTwoFour :: Board -> Picture
printPositionTwoFour board |pieceHere (2,4) board == True && getColour (2,4) board == Black = Pictures[
											Color blue (Translate (-150) (-50) (rectangleSolid 75 75)),
											Color black (Translate (-150) (-50) (circleSolid 35))
											]
		       |pieceHere (2,4) board == True && getColour (2,4) board == White = Pictures[
											Color blue (Translate (-150) (-50) (rectangleSolid 75 75)),
											Color white (Translate (-150) (-50) (circleSolid 35))
											]
		       |otherwise = Color blue (Translate (-150) (-50) (rectangleSolid 75 75))

printPositionTwoFive :: Board -> Picture
printPositionTwoFive board |pieceHere (2,5) board == True && getColour (2,5) board == Black = Pictures[
											Color blue (Translate (-150) (-150) (rectangleSolid 75 75)),
											Color black (Translate (-150) (-150) (circleSolid 35))
											]
		       |pieceHere (2,5) board == True && getColour (2,5) board == White = Pictures[
											Color blue (Translate (-150) (-150) (rectangleSolid 75 75)),
											Color white (Translate (-150) (-150) (circleSolid 35))
											]
		       |otherwise = Color blue (Translate (-150) (-150) (rectangleSolid 75 75))

printPositionTwoSix :: Board -> Picture
printPositionTwoSix board |pieceHere (2,6) board == True && getColour (2,6) board == Black = Pictures[
											Color blue (Translate (-150) (-250) (rectangleSolid 75 75)),
											Color black (Translate (-150) (-250) (circleSolid 35))
											]
		       |pieceHere (2,6) board == True && getColour (2,6) board == White = Pictures[
											Color blue (Translate (-150) (-250) (rectangleSolid 75 75)),
											Color white (Translate (-150) (-250) (circleSolid 35))
											]
		       |otherwise = Color blue (Translate (-150) (-250) (rectangleSolid 75 75))
	

--column 3

printPositionThreeOne :: Board -> Picture
printPositionThreeOne board |pieceHere (3,1) board == True && getColour (3,1) board == Black = Pictures[
											Color blue (Translate (-50) 250 (rectangleSolid 75 75)),
											Color black (Translate (-50) 250 (circleSolid 35))
											]
		       |pieceHere (3,1) board == True && getColour (3,1) board == White = Pictures[
											Color blue (Translate (-50) 250 (rectangleSolid 75 75)),
											Color white (Translate (-50) 250 (circleSolid 35))
											]
		       |otherwise = Color blue (Translate (-50) 250 (rectangleSolid 75 75))

printPositionThreeTwo :: Board -> Picture
printPositionThreeTwo board |pieceHere (3,2) board == True && getColour (3,2) board == Black = Pictures[
											Color blue (Translate (-50) 150 (rectangleSolid 75 75)),
											Color black (Translate (-50) 150 (circleSolid 35))
											]
		       |pieceHere (3,2) board == True && getColour (3,2) board == White = Pictures[
											Color blue (Translate (-50) 150 (rectangleSolid 75 75)),
											Color white (Translate (-50) 150 (circleSolid 35))
											]
		       |otherwise = Color blue (Translate (-50) 150 (rectangleSolid 75 75))

printPositionThreeThree :: Board -> Picture
printPositionThreeThree board |pieceHere (3,3) board == True && getColour (3,3) board == Black = Pictures[
											Color blue (Translate (-50) 50 (rectangleSolid 75 75)),
											Color black (Translate (-50) 50 (circleSolid 35))
											]
		       |pieceHere (3,3) board == True && getColour (3,3) board == White = Pictures[
											Color blue (Translate (-50) 50 (rectangleSolid 75 75)),
											Color white (Translate (-50) 50 (circleSolid 35))
											]
		       |otherwise = Color blue (Translate (-50) 50 (rectangleSolid 75 75))

printPositionThreeFour :: Board -> Picture
printPositionThreeFour board |pieceHere (3,4) board == True && getColour (3,4) board == Black = Pictures[
											Color blue (Translate (-50) (-50) (rectangleSolid 75 75)),
											Color black (Translate (-50) (-50) (circleSolid 35))
											]
		       |pieceHere (3,4) board == True && getColour (3,4) board == White = Pictures[
											Color blue (Translate (-50) (-50) (rectangleSolid 75 75)),
											Color white (Translate (-50) (-50) (circleSolid 35))
											]
		       |otherwise = Color blue (Translate (-50) (-50) (rectangleSolid 75 75))

printPositionThreeFive :: Board -> Picture
printPositionThreeFive board |pieceHere (3,5) board == True && getColour (3,5) board == Black = Pictures[
											Color blue (Translate (-50) (-150) (rectangleSolid 75 75)),
											Color black (Translate (-50) (-150) (circleSolid 35))
											]
		       |pieceHere (3,5) board == True && getColour (3,5) board == White = Pictures[
											Color blue (Translate (-50) (-150) (rectangleSolid 75 75)),
											Color white (Translate (-50) (-150) (circleSolid 35))
											]
		       |otherwise = Color blue (Translate (-50) (-150) (rectangleSolid 75 75))

printPositionThreeSix :: Board -> Picture
printPositionThreeSix board |pieceHere (3,6) board == True && getColour (3,6) board == Black = Pictures[
											Color blue (Translate (-50) (-250) (rectangleSolid 75 75)),
											Color black (Translate (-50) (-250) (circleSolid 35))
											]
		       |pieceHere (3,6) board == True && getColour (3,6) board == White = Pictures[
											Color blue (Translate (-50) (-250) (rectangleSolid 75 75)),
											Color white (Translate (-50) (-250) (circleSolid 35))
											]
		       |otherwise = Color blue (Translate (-50) (-250) (rectangleSolid 75 75))


--column 4

printPositionFourOne :: Board -> Picture
printPositionFourOne board |pieceHere (4,1) board == True && getColour (4,1) board == Black = Pictures[
											Color blue (Translate 50 250 (rectangleSolid 75 75)),
											Color black (Translate 50 250 (circleSolid 35))
											]
		       |pieceHere (4,1) board == True && getColour (4,1) board == White = Pictures[
											Color blue (Translate 50 250 (rectangleSolid 75 75)),
											Color white (Translate 50 250 (circleSolid 35))
											]
		       |otherwise = Color blue (Translate 50 250 (rectangleSolid 75 75))

printPositionFourTwo :: Board -> Picture
printPositionFourTwo board |pieceHere (4,2) board == True && getColour (4,2) board == Black = Pictures[
											Color blue (Translate 50 150 (rectangleSolid 75 75)),
											Color black (Translate 50 150 (circleSolid 35))
											]
		       |pieceHere (4,2) board == True && getColour (4,2) board == White = Pictures[
											Color blue (Translate 50 150 (rectangleSolid 75 75)),
											Color white (Translate 50 150 (circleSolid 35))
											]
		       |otherwise = Color blue (Translate 50 150 (rectangleSolid 75 75))

printPositionFourThree :: Board -> Picture
printPositionFourThree board |pieceHere (4,3) board == True && getColour (4,3) board == Black = Pictures[
											Color blue (Translate 50 50 (rectangleSolid 75 75)),
											Color black (Translate 50 50 (circleSolid 35))
											]
		       |pieceHere (4,3) board == True && getColour (4,3) board == White = Pictures[
											Color blue (Translate 50 50 (rectangleSolid 75 75)),
											Color white (Translate 50 50 (circleSolid 35))
											]
		       |otherwise = Color blue (Translate 50 50 (rectangleSolid 75 75))

printPositionFourFour :: Board -> Picture
printPositionFourFour board |pieceHere (4,4) board == True && getColour (4,4) board == Black = Pictures[
											Color blue (Translate 50 (-50) (rectangleSolid 75 75)),
											Color black (Translate 50 (-50) (circleSolid 35))
											]
		       |pieceHere (4,4) board == True && getColour (4,4) board == White = Pictures[
											Color blue (Translate 50 (-50) (rectangleSolid 75 75)),
											Color white (Translate 50 (-50) (circleSolid 35))
											]
		       |otherwise = Color blue (Translate 50 (-50) (rectangleSolid 75 75))

printPositionFourFive :: Board -> Picture
printPositionFourFive board |pieceHere (4,5) board == True && getColour (4,5) board == Black = Pictures[
											Color blue (Translate 50 (-150) (rectangleSolid 75 75)),
											Color black (Translate 50 (-150) (circleSolid 35))
											]
		       |pieceHere (4,5) board == True && getColour (4,5) board == White = Pictures[
											Color blue (Translate 50 (-150) (rectangleSolid 75 75)),
											Color white (Translate 50 (-150) (circleSolid 35))
											]
		       |otherwise = Color blue (Translate 50 (-150) (rectangleSolid 75 75))

printPositionFourSix :: Board -> Picture
printPositionFourSix board |pieceHere (4,6) board == True && getColour (4,6) board == Black = Pictures[
											Color blue (Translate 50 (-250) (rectangleSolid 75 75)),
											Color black (Translate 50 (-250) (circleSolid 35))
											]
		       |pieceHere (4,6) board == True && getColour (4,6) board == White = Pictures[
											Color blue (Translate 50 (-250) (rectangleSolid 75 75)),
											Color white (Translate 50 (-250) (circleSolid 35))
											]
		       |otherwise = Color blue (Translate 50 (-250) (rectangleSolid 75 75))


--column 5

printPositionFiveOne :: Board -> Picture
printPositionFiveOne board |pieceHere (5,1) board == True && getColour (5,1) board == Black = Pictures[
											Color blue (Translate 150 250 (rectangleSolid 75 75)),
											Color black (Translate 150 250 (circleSolid 35))
											]
		       |pieceHere (5,1) board == True && getColour (5,1) board == White = Pictures[
											Color blue (Translate 150 250 (rectangleSolid 75 75)),
											Color white (Translate 150 250 (circleSolid 35))
											]
		       |otherwise = Color blue (Translate 150 250 (rectangleSolid 75 75))

printPositionFiveTwo :: Board -> Picture
printPositionFiveTwo board |pieceHere (5,2) board == True && getColour (5,2) board == Black = Pictures[
											Color blue (Translate 150 150 (rectangleSolid 75 75)),
											Color black (Translate 150 150 (circleSolid 35))
											]
		       |pieceHere (5,2) board == True && getColour (5,2) board == White = Pictures[
											Color blue (Translate 150 150 (rectangleSolid 75 75)),
											Color white (Translate 150 150 (circleSolid 35))
											]
		       |otherwise = Color blue (Translate 150 150 (rectangleSolid 75 75))

printPositionFiveThree :: Board -> Picture
printPositionFiveThree board |pieceHere (5,3) board == True && getColour (5,3) board == Black = Pictures[
											Color blue (Translate 150 50 (rectangleSolid 75 75)),
											Color black (Translate 150 50 (circleSolid 35))
											]
		       |pieceHere (5,3) board == True && getColour (5,3) board == White = Pictures[
											Color blue (Translate 150 50 (rectangleSolid 75 75)),
											Color white (Translate 150 50 (circleSolid 35))
											]
		       |otherwise = Color blue (Translate 150 50 (rectangleSolid 75 75))

printPositionFiveFour :: Board -> Picture
printPositionFiveFour board |pieceHere (5,4) board == True && getColour (5,4) board == Black = Pictures[
											Color blue (Translate 150 (-50) (rectangleSolid 75 75)),
											Color black (Translate 150 (-50) (circleSolid 35))
											]
		       |pieceHere (5,4) board == True && getColour (5,4) board == White = Pictures[
											Color blue (Translate 150 (-50) (rectangleSolid 75 75)),
											Color white (Translate 150 (-50) (circleSolid 35))
											]
		       |otherwise = Color blue (Translate 150 (-50) (rectangleSolid 75 75))

printPositionFiveFive :: Board -> Picture
printPositionFiveFive board |pieceHere (5,5) board == True && getColour (5,5) board == Black = Pictures[
											Color blue (Translate 150 (-150) (rectangleSolid 75 75)),
											Color black (Translate 150 (-150) (circleSolid 35))
											]
		       |pieceHere (5,5) board == True && getColour (5,5) board == White = Pictures[
											Color blue (Translate 150 (-150) (rectangleSolid 75 75)),
											Color white (Translate 150 (-150) (circleSolid 35))
											]
		       |otherwise = Color blue (Translate 150 (-150) (rectangleSolid 75 75))

printPositionFiveSix :: Board -> Picture
printPositionFiveSix board |pieceHere (5,6) board == True && getColour (5,6) board == Black = Pictures[
											Color blue (Translate 150 (-250) (rectangleSolid 75 75)),
											Color black (Translate 150 (-250) (circleSolid 35))
											]
		       |pieceHere (5,6) board == True && getColour (5,6) board == White = Pictures[
											Color blue (Translate 150 (-250) (rectangleSolid 75 75)),
											Color white (Translate 150 (-250) (circleSolid 35))
											]
		       |otherwise = Color blue (Translate 150 (-250) (rectangleSolid 75 75))


--column 6

printPositionSixOne :: Board -> Picture
printPositionSixOne board |pieceHere (6,1) board == True && getColour (6,1) board == Black = Pictures[
											Color blue (Translate 250 250 (rectangleSolid 75 75)),
											Color black (Translate 250 250 (circleSolid 35))
											]
		       |pieceHere (6,1) board == True && getColour (6,1) board == White = Pictures[
											Color blue (Translate 250 250 (rectangleSolid 75 75)),
											Color white (Translate 250 250 (circleSolid 35))
											]
		       |otherwise = Color blue (Translate 250 250 (rectangleSolid 75 75))

printPositionSixTwo :: Board -> Picture
printPositionSixTwo board |pieceHere (6,2) board == True && getColour (6,2) board == Black = Pictures[
											Color blue (Translate 250 150 (rectangleSolid 75 75)),
											Color black (Translate 250 150 (circleSolid 35))
											]
		       |pieceHere (6,2) board == True && getColour (6,2) board == White = Pictures[
											Color blue (Translate 250 150 (rectangleSolid 75 75)),
											Color white (Translate 250 150 (circleSolid 35))
											]
		       |otherwise = Color blue (Translate 250 150 (rectangleSolid 75 75))

printPositionSixThree :: Board -> Picture
printPositionSixThree board |pieceHere (6,3) board == True && getColour (6,3) board == Black = Pictures[
											Color blue (Translate 250 50 (rectangleSolid 75 75)),
											Color black (Translate 250 50 (circleSolid 35))
											]
		       |pieceHere (6,3) board == True && getColour (6,3) board == White = Pictures[
											Color blue (Translate 250 50 (rectangleSolid 75 75)),
											Color white (Translate 250 50 (circleSolid 35))
											]
		       |otherwise = Color blue (Translate 250 50 (rectangleSolid 75 75))

printPositionSixFour :: Board -> Picture
printPositionSixFour board |pieceHere (6,4) board == True && getColour (6,4) board == Black = Pictures[
											Color blue (Translate 250 (-50) (rectangleSolid 75 75)),
											Color black (Translate 250 (-50) (circleSolid 35))
											]
		       |pieceHere (6,4) board == True && getColour (6,4) board == White = Pictures[
											Color blue (Translate 250 (-50) (rectangleSolid 75 75)),
											Color white (Translate 250 (-50) (circleSolid 35))
											]
		       |otherwise = Color blue (Translate 250 (-50) (rectangleSolid 75 75))

printPositionSixFive :: Board -> Picture
printPositionSixFive board |pieceHere (6,5) board == True && getColour (6,5) board == Black = Pictures[
											Color blue (Translate 250 (-150) (rectangleSolid 75 75)),
											Color black (Translate 250 (-150) (circleSolid 35))
											]
		       |pieceHere (6,5) board == True && getColour (6,5) board == White = Pictures[
											Color blue (Translate 250 (-150) (rectangleSolid 75 75)),
											Color white (Translate 250 (-150) (circleSolid 35))
											]
		       |otherwise = Color blue (Translate 250 (-150) (rectangleSolid 75 75))

printPositionSixSix :: Board -> Picture
printPositionSixSix board |pieceHere (6,6) board == True && getColour (6,6) board == Black = Pictures[
											Color blue (Translate 250 (-250) (rectangleSolid 75 75)),
											Color black (Translate 250 (-250) (circleSolid 35))
											]
		       |pieceHere (6,6) board == True && getColour (6,6) board == White = Pictures[
											Color blue (Translate 250 (-250) (rectangleSolid 75 75)),
											Color white (Translate 250 (-250) (circleSolid 35))
											]
		       |otherwise = Color blue (Translate 250 (-250) (rectangleSolid 75 75))




											

pieceHere :: Position -> Board -> Bool
pieceHere pos board = elem pos (map (\ posi -> sel1  posi) piecesOnBoard)
		      where piecesOnBoard = pieces board

getColour :: Position -> Board -> Col
getColour pos board = sel2 (head[x | x <- pieces board, sel1 x == pos])

