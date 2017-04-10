module Draw(drawWorld, convertToColumnMultiRow, convertToColumnMultiColumn) where

import Graphics.Gloss
import Board
import Data.Tuple.Select
import Debug.Trace

-- Given a world state, return a Picture which will render the world state.
drawWorld :: World -> Picture
drawWorld w |(won w) == True && (winner w) == Black = Pictures[
					Color white (Translate (-170) 325 (Scale 0.5 0.5 (Text "Black Wins!"))),
					printBoard w,
					printButtons w
					]
	    |(won w) == True && (winner w) == White  = Pictures[
					Color white (Translate (-170) 325 (Scale 0.5 0.5 (Text "White Wins!"))),
					printBoard w,
					printButtons w
					]
	    |otherwise = Pictures[
											printBoard w,
											printButtons w
											]

printButtons :: World -> Picture
printButtons w |length (pieces (board w)) >0 =  Pictures [
																											printUndoButton,
																											printRestartButton
																											]
							 |otherwise = Pictures [printRestartButton]

printRestartButton ::Picture
printRestartButton = Pictures [
													Color violet (Translate (-95) (-360) (rectangleSolid 80 40)),
													Color white (Translate (-128) (-365) (Scale 0.15 0.15 (Text "Restart!")))
													]

printUndoButton ::Picture
printUndoButton  = Pictures [
											Color violet (Translate (-200) (-360) (rectangleSolid 60 40)),
											Color white (Translate (-224) (-365) (Scale 0.15 0.15 (Text "Undo!")))
											]

printBoard :: World -> Picture
printBoard w = Pictures [square x y (board w)| x <- reverse(generateList (board w)), y <- (generateList (board w))]

square :: Float -> Float -> Board -> Picture
square x y board |pieceHere ((convertToColumnMultiColumn x board), (convertToColumnMultiRow y board)) board == True && getColour ((convertToColumnMultiColumn x board), (convertToColumnMultiRow y board)) board == Black = Pictures[
																																																																											Color blue (Translate x y (rectangleSolid (getSquareSize board) (getSquareSize board))),
																																																																											Color black (Translate x y (circleSolid (getCircleSize board)))
																																																																											]
					       |pieceHere ((convertToColumnMultiColumn x board), (convertToColumnMultiRow y board)) board == True && getColour ((convertToColumnMultiColumn x board), (convertToColumnMultiRow y board)) board == White = Pictures[
 																																																																											Color blue (Translate x y (rectangleSolid (getSquareSize board) (getSquareSize board))),
 																																																																											Color white (Translate x y (circleSolid (getCircleSize board )))
 																																																																											]
					       |otherwise = Color blue (Translate x y  (rectangleSolid (getSquareSize board) (getSquareSize board)))

getSquareSize :: Board -> Float
getSquareSize board |size board == 7 = 60
										|size board == 8 = 55
										|size board == 9 = 50
										|size board == 10 = 45
										|size board == 11 = 40
										|size board == 12 = 35
										|size board == 13 = 30
										|size board == 14 = 25
										|size board == 15 = 25
										|size board == 16 = 25
										|size board == 17 = 25
										|size board == 18 = 25
										|size board == 19 = 25
										|otherwise = 75


getCircleSize :: Board -> Float
getCircleSize board |size board == 7 = 30
										|size board == 8 = 28
										|size board == 9 = 25
										|size board == 10 = 23
										|size board == 11 = 20
										|size board == 12 = 18
										|size board == 13 = 15
										|size board == 14 = 12
										|size board == 15 = 12
										|size board == 16 = 12
										|size board == 17 = 12
										|size board == 18 = 12
										|size board == 19 = 12
										|otherwise = 35

generateList :: Board -> [Float]
generateList board |size board == 7 = [250, 167, 84, 1, -82, -165, -248]
									 |size board == 8 = [250, 179, 108, 37, -34, -105, -176, -247]
									 |size board == 9 = [250, 185, 123, 61, -1, -63, -125, -187, -249]
									 |size board == 10 = [250, 195, 140, 85, 30, -25, -80, -135, -190, -245]
									 |size board == 11 = [250, 200, 150, 100, 50, 0, -50, -100, -150, -200, -250]
									 |size board == 12 = [250, 205, 160, 115, 70, 25, -20, -65, -110, -155, -200, -245]
									 |size board == 13 = [250, 209, 168, 127, 86, 45, 4, -37, -78, -119, -160, -201, -242]
									 |size board == 14 = [250, 212, 174, 136, 98, 60, 22, -16, -54, -92, -130, -168, -206, -244]
									 |size board == 15 = [260, 222, 184, 146, 108, 70, 32, -6, -44, -82, -120, -158, -196, -234, -272]
									 |size board == 16 = [270, 232, 194, 156, 118, 80, 42, 4, -34, -72, -110, -148, -186, -224, -262, -300]
									 |size board == 17 = [300, 262, 224, 186, 148, 110, 72, 34, -4, -42, -80, -118, -156, -194, -232, -270, -307]
									 |size board == 18 = [330, 292, 254, 216, 178, 140, 102, 64, 26, -12, -50, -88, -126, -164, -202, -240, -277, -315]
									 |size board == 19 = [365, 327, 289, 251, 213, 175, 137, 99, 61, 23, -15, -53, -91, -129, -167, -205, -242, -280, -318]
									 |otherwise = [250, 150, 50, -50, -150, -250]


convertToColumnMultiColumn :: Float -> Board -> Int
convertToColumnMultiColumn x board |length (generateList board) > 0 && x<((reverse(generateList board)!!0)+((getSquareSize board)/2)) && x>((reverse(generateList board)!!0)-((getSquareSize board)/2)) = 1
							               |length (generateList board) > 1 && x<((reverse(generateList board)!!1)+((getSquareSize board)/2)) && x>((reverse(generateList board)!!1)-((getSquareSize board)/2)) = 2
							               |length (generateList board) > 2 && x<((reverse(generateList board)!!2)+((getSquareSize board)/2)) && x>((reverse(generateList board)!!2)-((getSquareSize board)/2))  = 3
							               |length (generateList board) > 3 && x<((reverse(generateList board)!!3)+((getSquareSize board)/2)) && x>((reverse(generateList board)!!3)-((getSquareSize board)/2))  = 4
							               |length (generateList board) > 4 && x<((reverse(generateList board)!!4)+((getSquareSize board)/2)) && x>((reverse(generateList board)!!4)-((getSquareSize board)/2))  = 5
							               |length (generateList board) > 5 && x<((reverse(generateList board)!!5)+((getSquareSize board)/2)) && x>((reverse(generateList board)!!5)-((getSquareSize board)/2))  = 6
														 |length (generateList board) > 6 && x<((reverse(generateList board)!!6)+((getSquareSize board)/2)) && x>((reverse(generateList board)!!6)-((getSquareSize board)/2))  = 7
														 |length (generateList board) > 7 && x<((reverse(generateList board)!!7)+((getSquareSize board)/2)) && x>((reverse(generateList board)!!7)-((getSquareSize board)/2))  = 8
														 |length (generateList board) > 8 && x<((reverse(generateList board)!!8)+((getSquareSize board)/2)) && x>((reverse(generateList board)!!8)-((getSquareSize board)/2))  = 9
														 |length (generateList board) > 9 && x<((reverse(generateList board)!!9)+((getSquareSize board)/2)) && x>((reverse(generateList board)!!9)-((getSquareSize board)/2))  = 10
														 |length (generateList board) > 10 && x<((reverse(generateList board)!!10)+((getSquareSize board)/2)) && x>((reverse(generateList board)!!10)-((getSquareSize board)/2))  = 11
														 |length (generateList board) > 11 && x<((reverse(generateList board)!!11)+((getSquareSize board)/2)) && x>((reverse(generateList board)!!11)-((getSquareSize board)/2))  = 12
														 |length (generateList board) > 12 && x<((reverse(generateList board)!!12)+((getSquareSize board)/2)) && x>((reverse(generateList board)!!12)-((getSquareSize board)/2))  = 13
														 |length (generateList board) > 13 && x<((reverse(generateList board)!!13)+((getSquareSize board)/2)) && x>((reverse(generateList board)!!13)-((getSquareSize board)/2))  = 14
														 |length (generateList board) > 14 && x<((reverse(generateList board)!!14)+((getSquareSize board)/2)) && x>((reverse(generateList board)!!14)-((getSquareSize board)/2))  = 15
														 |length (generateList board) > 15 && x<((reverse(generateList board)!!15)+((getSquareSize board)/2)) && x>((reverse(generateList board)!!15)-((getSquareSize board)/2))  = 16
														 |length (generateList board) > 16 && x<((reverse(generateList board)!!16)+((getSquareSize board)/2)) && x>((reverse(generateList board)!!16)-((getSquareSize board)/2))  = 17
														 |length (generateList board) > 17 && x<((reverse(generateList board)!!17)+((getSquareSize board)/2)) && x>((reverse(generateList board)!!17)-((getSquareSize board)/2))  = 18
														 |length (generateList board) > 18 && x<((reverse(generateList board)!!18)+((getSquareSize board)/2)) && x>((reverse(generateList board)!!18)-((getSquareSize board)/2))  = 19
							               |otherwise = 0




convertToColumnMultiRow :: Float -> Board -> Int
convertToColumnMultiRow y board |length (generateList board) > 0 && y<(((generateList board)!!0)+((getSquareSize board)/2)) && y>(((generateList board)!!0)-((getSquareSize board)/2)) = 1
								                |length (generateList board) > 1 && y<(((generateList board)!!1)+((getSquareSize board)/2)) && y>(((generateList board)!!1)-((getSquareSize board)/2)) = 2
								                |length (generateList board) > 2 && y<(((generateList board)!!2)+((getSquareSize board)/2)) && y>(((generateList board)!!2)-((getSquareSize board)/2))  = 3
								                |length (generateList board) > 3 && y<(((generateList board)!!3)+((getSquareSize board)/2)) && y>(((generateList board)!!3)-((getSquareSize board)/2))  = 4
								                |length (generateList board) > 4 && y<(((generateList board)!!4)+((getSquareSize board)/2)) && y>(((generateList board)!!4)-((getSquareSize board)/2))  = 5
								                |length (generateList board) > 5 && y<(((generateList board)!!5)+((getSquareSize board)/2)) && y>(((generateList board)!!5)-((getSquareSize board)/2))  = 6
															  |length (generateList board) > 6 && y<(((generateList board)!!6)+((getSquareSize board)/2)) && y>(((generateList board)!!6)-((getSquareSize board)/2))  = 7
															  |length (generateList board) > 7 && y<(((generateList board)!!7)+((getSquareSize board)/2)) && y>(((generateList board)!!7)-((getSquareSize board)/2))  = 8
															  |length (generateList board) > 8 && y<(((generateList board)!!8)+((getSquareSize board)/2)) && y>(((generateList board)!!8)-((getSquareSize board)/2))  = 9
															  |length (generateList board) > 9 && y<(((generateList board)!!9)+((getSquareSize board)/2)) && y>(((generateList board)!!9)-((getSquareSize board)/2))  = 10
															  |length (generateList board) > 10 && y<(((generateList board)!!10)+((getSquareSize board)/2)) && y>(((generateList board)!!10)-((getSquareSize board)/2))  = 11
															  |length (generateList board) > 11 && y<(((generateList board)!!11)+((getSquareSize board)/2)) && y>(((generateList board)!!11)-((getSquareSize board)/2))  = 12
															  |length (generateList board) > 12 && y<(((generateList board)!!12)+((getSquareSize board)/2)) && y>(((generateList board)!!12)-((getSquareSize board)/2))  = 13
															  |length (generateList board) > 13 && y<(((generateList board)!!13)+((getSquareSize board)/2)) && y>(((generateList board)!!13)-((getSquareSize board)/2))  = 14
															  |length (generateList board) > 14 && y<(((generateList board)!!14)+((getSquareSize board)/2)) && y>(((generateList board)!!14)-((getSquareSize board)/2))  = 15
															  |length (generateList board) > 15 && y<(((generateList board)!!15)+((getSquareSize board)/2)) && y>(((generateList board)!!15)-((getSquareSize board)/2))  = 16
															  |length (generateList board) > 16 && y<(((generateList board)!!16)+((getSquareSize board)/2)) && y>(((generateList board)!!16)-((getSquareSize board)/2)) = 17
															  |length (generateList board) > 17 && y<(((generateList board)!!17)+((getSquareSize board)/2)) && y>(((generateList board)!!17)-((getSquareSize board)/2))  = 18
															  |length (generateList board) > 18 && y<(((generateList board)!!18)+((getSquareSize board)/2)) && y>(((generateList board)!!18)-((getSquareSize board)/2))  = 19
								                |otherwise = 0

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
