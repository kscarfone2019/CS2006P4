module Draw(drawWorld) where

import Graphics.Gloss
import Board
import Data.Tuple.Select

-- Given a world state, return a Picture which will render the world state.
-- Currently just draws a single blue circle as a placeholder.
--
-- This will need to extract the Board from the world state and draw it
-- as a grid plus pieces.
drawWorld :: World -> Picture

--Color black (Translate 50 (-250) (circleSolid 40)),
--Color white (Translate 50 (-250) (circleSolid 40)),

drawWorld w = Pictures [
		--column 1
		printPositionOneOne (board w),
		printPositionTwoOne (board w),
		printPositionThreeOne (board w),
		printPositionFourOne (board w),
		printPositionFiveOne (board w),
		printPositionSixOne(board w),

		--column 2
		printPositionOneTwo (board w),
		printPositionTwoTwo (board w),
		printPositionThreeTwo (board w),
		printPositionFourTwo (board w),
		printPositionFiveTwo (board w),
		printPositionSixTwo (board w),
		
		--column 3
		printPositionOneThree (board w),
		printPositionTwoThree (board w),
		printPositionThreeThree (board w),
		printPositionFourThree (board w),
		printPositionFiveThree (board w),
		printPositionSixThree (board w),

		--column 4
		Color yellow (Translate 50 250 (rectangleSolid 75 75)),
		Color yellow (Translate 50 150 (rectangleSolid 75 75)),
		Color yellow (Translate 50 50 (rectangleSolid 75 75)),
		Color yellow (Translate 50 (-50) (rectangleSolid 75 75)),
		Color yellow (Translate 50 (-150) (rectangleSolid 75 75)),
		Color yellow (Translate 50 (-250) (rectangleSolid 75 75)),

		--column 5
		Color yellow (Translate 150 250 (rectangleSolid 75 75)),
		Color yellow (Translate 150 150 (rectangleSolid 75 75)),
		Color yellow (Translate 150 50 (rectangleSolid 75 75)),
		Color yellow (Translate 150 (-50) (rectangleSolid 75 75)),
		Color yellow (Translate 150 (-150) (rectangleSolid 75 75)),
		Color yellow (Translate 150 (-250) (rectangleSolid 75 75)),

		--column 6
		Color yellow (Translate 250 250 (rectangleSolid 75 75)),
		Color yellow (Translate 250 150 (rectangleSolid 75 75)),
		Color yellow (Translate 250 50 (rectangleSolid 75 75)),
		Color yellow (Translate 250 (-50) (rectangleSolid 75 75)),
		Color yellow (Translate 250 (-150) (rectangleSolid 75 75)),
		Color yellow (Translate 250 (-250) (rectangleSolid 75 75))
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

printPositionTwoOne :: Board -> Picture
printPositionTwoOne board |pieceHere (1,2) board == True && getColour (1,2) board == Black = Pictures[
											Color blue (Translate (-250) 150 (rectangleSolid 75 75)),
											Color black (Translate (-250) 250 (circleSolid 35))
											]
		       |pieceHere (1,2) board == True && getColour (1,2) board == White = Pictures[
											Color blue (Translate (-250) 150 (rectangleSolid 75 75)),
											Color white (Translate (-250) 250 (circleSolid 35))
											]
		       |otherwise = Color blue (Translate (-250) 150 (rectangleSolid 75 75))

printPositionThreeOne :: Board -> Picture
printPositionThreeOne board |pieceHere (1,3) board == True && getColour (1,3) board == Black = Pictures[
											Color blue (Translate (-250) 50 (rectangleSolid 75 75)),
											Color black (Translate (-250) 250 (circleSolid 35))
											]
		       |pieceHere (1,3) board == True && getColour (1,3) board == White = Pictures[
											Color blue (Translate (-250) 50 (rectangleSolid 75 75)),
											Color white (Translate (-250) 250 (circleSolid 35))
											]
		       |otherwise = Color blue (Translate (-250) 50 (rectangleSolid 75 75))

printPositionFourOne :: Board -> Picture
printPositionFourOne board |pieceHere (1,4) board == True && getColour (1,4) board == Black = Pictures[
											Color blue (Translate (-250) (-50) (rectangleSolid 75 75)),
											Color black (Translate (-250) 250 (circleSolid 35))
											]
		       |pieceHere (1,4) board == True && getColour (1,4) board == White = Pictures[
											Color blue (Translate (-250) (-50) (rectangleSolid 75 75)),
											Color white (Translate (-250) 250 (circleSolid 35))
											]
		       |otherwise = Color blue (Translate (-250) (-50) (rectangleSolid 75 75))

printPositionFiveOne :: Board -> Picture
printPositionFiveOne board |pieceHere (1,5) board == True && getColour (1,5) board == Black = Pictures[
											Color blue (Translate (-250) (-150) (rectangleSolid 75 75)),
											Color black (Translate (-250) 250 (circleSolid 35))
											]
		       |pieceHere (1,5) board == True && getColour (1,5) board == White = Pictures[
											Color blue (Translate (-250) (-150) (rectangleSolid 75 75)),
											Color white (Translate (-250) 250 (circleSolid 35))
											]
		       |otherwise = Color blue (Translate (-250) (-150) (rectangleSolid 75 75))

printPositionSixOne :: Board -> Picture
printPositionSixOne board |pieceHere (1,6) board == True && getColour (1,6) board == Black = Pictures[
											Color blue (Translate (-250) (-250) (rectangleSolid 75 75)),
											Color black (Translate (-250) 250 (circleSolid 35))
											]
		       |pieceHere (1,6) board == True && getColour (1,6) board == White = Pictures[
											Color blue (Translate (-250) (-250) (rectangleSolid 75 75)),
											Color white (Translate (-250) 250 (circleSolid 35))
											]
		       |otherwise = Color blue (Translate (-250) (-250) (rectangleSolid 75 75))

--column 2

printPositionOneTwo :: Board -> Picture
printPositionOneTwo board |pieceHere (2,1) board == True && getColour (2,1) board == Black = Pictures[
											Color blue (Translate (-150) 250 (rectangleSolid 75 75)),
											Color black (Translate (-250) 250 (circleSolid 35))
											]
		       |pieceHere (2,1) board == True && getColour (2,1) board == White = Pictures[
											Color blue (Translate (-150) 250 (rectangleSolid 75 75)),
											Color white (Translate (-250) 250 (circleSolid 35))
											]
		       |otherwise = Color blue (Translate (-150) 250 (rectangleSolid 75 75))

printPositionTwoTwo :: Board -> Picture
printPositionTwoTwo board |pieceHere (2,2) board == True && getColour (2,2) board == Black = Pictures[
											Color blue (Translate (-150) 150 (rectangleSolid 75 75)),
											Color black (Translate (-250) 250 (circleSolid 35))
											]
		       |pieceHere (2,2) board == True && getColour (2,2) board == White = Pictures[
											Color blue (Translate (-150) 150 (rectangleSolid 75 75)),
											Color white (Translate (-250) 250 (circleSolid 35))
											]
		       |otherwise = Color blue (Translate (-150) 150 (rectangleSolid 75 75))

printPositionThreeTwo :: Board -> Picture
printPositionThreeTwo board |pieceHere (2,3) board == True && getColour (2,3) board == Black = Pictures[
											Color blue (Translate (-150) 50 (rectangleSolid 75 75)),
											Color black (Translate (-250) 250 (circleSolid 35))
											]
		       |pieceHere (2,3) board == True && getColour (2,3) board == White = Pictures[
											Color blue (Translate (-150) 50 (rectangleSolid 75 75)),
											Color white (Translate (-250) 250 (circleSolid 35))
											]
		       |otherwise = Color blue (Translate (-150) 50 (rectangleSolid 75 75))

printPositionFourTwo :: Board -> Picture
printPositionFourTwo board |pieceHere (2,4) board == True && getColour (2,4) board == Black = Pictures[
											Color blue (Translate (-150) (-50) (rectangleSolid 75 75)),
											Color black (Translate (-250) 250 (circleSolid 35))
											]
		       |pieceHere (2,4) board == True && getColour (2,4) board == White = Pictures[
											Color blue (Translate (-150) (-50) (rectangleSolid 75 75)),
											Color white (Translate (-250) 250 (circleSolid 35))
											]
		       |otherwise = Color blue (Translate (-150) (-50) (rectangleSolid 75 75))

printPositionFiveTwo :: Board -> Picture
printPositionFiveTwo board |pieceHere (2,5) board == True && getColour (2,5) board == Black = Pictures[
											Color blue (Translate (-150) (-150) (rectangleSolid 75 75)),
											Color black (Translate (-250) 250 (circleSolid 35))
											]
		       |pieceHere (2,5) board == True && getColour (2,5) board == White = Pictures[
											Color blue (Translate (-150) (-150) (rectangleSolid 75 75)),
											Color white (Translate (-250) 250 (circleSolid 35))
											]
		       |otherwise = Color blue (Translate (-150) (-150) (rectangleSolid 75 75))

printPositionSixTwo :: Board -> Picture
printPositionSixTwo board |pieceHere (2,6) board == True && getColour (2,6) board == Black = Pictures[
											Color blue (Translate (-150) (-250) (rectangleSolid 75 75)),
											Color black (Translate (-250) 250 (circleSolid 35))
											]
		       |pieceHere (2,6) board == True && getColour (2,6) board == White = Pictures[
											Color blue (Translate (-150) (-250) (rectangleSolid 75 75)),
											Color white (Translate (-250) 250 (circleSolid 35))
											]
		       |otherwise = Color blue (Translate (-150) (-250) (rectangleSolid 75 75))

		

	

		
		

--column 3

printPositionOneThree :: Board -> Picture
printPositionOneThree board |pieceHere (3,1) board == True && getColour (3,1) board == Black = Pictures[
											Color blue (Translate (-50) 250 (rectangleSolid 75 75)),
											Color black (Translate (-250) 250 (circleSolid 35))
											]
		       |pieceHere (3,1) board == True && getColour (3,1) board == White = Pictures[
											Color blue (Translate (-50) 250 (rectangleSolid 75 75)),
											Color white (Translate (-250) 250 (circleSolid 35))
											]
		       |otherwise = Color blue (Translate (-50) 250 (rectangleSolid 75 75))

printPositionTwoThree :: Board -> Picture
printPositionTwoThree board |pieceHere (3,2) board == True && getColour (3,2) board == Black = Pictures[
											Color blue (Translate (-50) 150 (rectangleSolid 75 75)),
											Color black (Translate (-250) 250 (circleSolid 35))
											]
		       |pieceHere (3,2) board == True && getColour (3,2) board == White = Pictures[
											Color blue (Translate (-50) 150 (rectangleSolid 75 75)),
											Color white (Translate (-250) 250 (circleSolid 35))
											]
		       |otherwise = Color blue (Translate (-50) 150 (rectangleSolid 75 75))

printPositionThreeThree :: Board -> Picture
printPositionThreeThree board |pieceHere (3,3) board == True && getColour (3,3) board == Black = Pictures[
											Color blue (Translate (-50) 50 (rectangleSolid 75 75)),
											Color black (Translate (-250) 250 (circleSolid 35))
											]
		       |pieceHere (3,3) board == True && getColour (3,3) board == White = Pictures[
											Color blue (Translate (-50) 50 (rectangleSolid 75 75)),
											Color white (Translate (-250) 250 (circleSolid 35))
											]
		       |otherwise = Color blue (Translate (-50) 50 (rectangleSolid 75 75))

printPositionFourThree :: Board -> Picture
printPositionFourThree board |pieceHere (3,4) board == True && getColour (3,4) board == Black = Pictures[
											Color blue (Translate (-50) (-50) (rectangleSolid 75 75)),
											Color black (Translate (-250) 250 (circleSolid 35))
											]
		       |pieceHere (3,4) board == True && getColour (3,4) board == White = Pictures[
											Color blue (Translate (-50) (-50) (rectangleSolid 75 75)),
											Color white (Translate (-250) 250 (circleSolid 35))
											]
		       |otherwise = Color blue (Translate (-50) (-50) (rectangleSolid 75 75))

printPositionFiveThree :: Board -> Picture
printPositionFiveThree board |pieceHere (3,5) board == True && getColour (3,5) board == Black = Pictures[
											Color blue (Translate (-50) (-150) (rectangleSolid 75 75)),
											Color black (Translate (-250) 250 (circleSolid 35))
											]
		       |pieceHere (3,5) board == True && getColour (3,5) board == White = Pictures[
											Color blue (Translate (-50) (-150) (rectangleSolid 75 75)),
											Color white (Translate (-250) 250 (circleSolid 35))
											]
		       |otherwise = Color blue (Translate (-50) (-150) (rectangleSolid 75 75))

printPositionSixThree :: Board -> Picture
printPositionSixThree board |pieceHere (3,6) board == True && getColour (3,6) board == Black = Pictures[
											Color blue (Translate (-50) (-250) (rectangleSolid 75 75)),
											Color black (Translate (-250) 250 (circleSolid 35))
											]
		       |pieceHere (3,6) board == True && getColour (3,6) board == White = Pictures[
											Color blue (Translate (-50) (-250) (rectangleSolid 75 75)),
											Color white (Translate (-250) 250 (circleSolid 35))
											]
		       |otherwise = Color blue (Translate (-50) (-250) (rectangleSolid 75 75))


											

pieceHere :: Position -> Board -> Bool
pieceHere pos board = elem pos (map (\ posi -> sel1  posi) piecesOnBoard)
		      where piecesOnBoard = pieces board

getColour :: Position -> Board -> Col
getColour pos board = sel2 (head[x | x <- pieces board, sel1 x == pos])

