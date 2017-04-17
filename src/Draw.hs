-- |Draw. All pictures are made in here.
module Draw(drawWorld, convertToColumnMultiRow, convertToColumnMultiColumn,showHint) where

--import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Game
import Board
import Data.Tuple.Select
import Debug.Trace
import AI

-- |Given a world state, a Picture which will render the world state is returned.
drawWorld :: World -- ^The current world.
									-> IO Picture -- ^The IO Picture to be shown.
drawWorld w |(won w) == True && (winner w) == Black = do return (Pictures[
																																		printBoard w,
																																		printButtons w,
																																		Color violet (Translate (-170) 345 (rectangleSolid 100000 80)),
																																		Color white (Translate (-170) 325 (Scale 0.5 0.5 (Text "Black Wins!")))
																																		])
	    |(won w) == True && (winner w) == White  = do return (Pictures[
																															printBoard w,
																															printButtons w,
																															Color violet (Translate (-170) 345 (rectangleSolid 100000 80)),
																															Color white (Translate (-170) 325 (Scale 0.5 0.5 (Text "White Wins!")))
																															])
	    |otherwise = do return (Pictures[
											printBoard w,
											printButtons w
											])

-- |Print the Picture for the Hint Button.
printHintButton :: Picture
printHintButton = Pictures[
											Color violet (Translate (400) (215) (rectangleSolid 60 55)),
											Color white (Translate (377) (220) (Scale 0.15 0.15 (Text "Show"))),
											Color white (Translate (381) (195) (Scale 0.15 0.15 (Text "Hint")))
											]

-- |Show the Picture for the hint on the screen.
showHint :: World -- ^The current world.
									-> Picture
showHint world = Pictures [
										Color violet (Translate (403) (155) (rectangleSolid 65 34)),
										Color white (Translate (377) (150) (Scale 0.15 0.15 (Text (show(getBestMoveForHint world)))))
											]

-- |get The best move available for the player.
getBestMoveForHint :: World -- ^The current world.
														-> Position
getBestMoveForHint world = (getBestMove 1 (buildTree (gen) (board world) (turn world)))

-- |Print the Picture for the Buttons.
printButtons :: World -- ^The curretn world.
											-> Picture
printButtons w |length (pieces (board w)) >0 =  Pictures [
																										printUndoButton,
																										printMostButtons w
																										]
							 |otherwise = Pictures [printMostButtons w]

-- |Print the Picture for the the majority of the buttons, if a hint has been requested then it prints out.
printMostButtons :: World -- ^The current world.
													-> Picture
printMostButtons w |hint w == True =  Pictures [
																							printRestartButton,
																							printIncreaseBoardSizeButton,
																							printBoardSizeButton,
																							printDecreaseBoardSizeButton,
																							printIncreaseLineButton,
																							printLineSizeButton w,
																							printDecreaseLineButton,
																							printSaveButton,
																							printLoadButton,
																							printHintButton,
																							printTimeButton w,
																							printBitmapButton,
																							showHint w
																							]
									|otherwise = Pictures [
																	printRestartButton,
																	printIncreaseBoardSizeButton,
																	printBoardSizeButton,
																	printDecreaseBoardSizeButton,
																	printIncreaseLineButton,
																	printLineSizeButton w,
																	printDecreaseLineButton,
																	printSaveButton,
																	printLoadButton,
																	printTimeButton w,
																	printBitmapButton,
																	printHintButton
																	]


-- |Show the Picture for the play with timer button on the screen.
printTimeButton :: World -> Picture
printTimeButton w |(time w) == True =  Pictures [
																										Color violet (Translate (403) (100) (rectangleSolid 65 34)),
																										Color white (Translate (374) (93) (Scale 0.15 0.15 (Text "Timer!"))),
																										printTimer w,
																										printPauseButton
																											]
											|otherwise = Pictures [
																			Color violet (Translate (403) (100) (rectangleSolid 65 34)),
																			Color white (Translate (374) (93) (Scale 0.15 0.15 (Text "Timer!")))
																				]




-- |Show the Picture for the play with timer button on the screen.
printBitmapButton :: Picture
printBitmapButton =  Pictures [
														Color violet (Translate (403) (15) (rectangleSolid 65 60)),
														Color white (Translate (375) (18) (Scale 0.13 0.13 (Text "Bitmap"))),
														Color white (Translate (372) (-2) (Scale 0.13 0.13 (Text "Pictures")))
															]


-- |Print the Picture for the Restart Button.
printRestartButton :: Picture
printRestartButton = Pictures [
													Color violet (Translate (-95) (-370) (rectangleSolid 80 40)),
													Color white (Translate (-128) (-375) (Scale 0.15 0.15 (Text "Restart!")))
													]

-- |Print the Picture for the Timer Button.
printTimer :: World -- ^The current world.
										-> Picture
printTimer world = Pictures [
													Color violet (Translate (43) (-370) (rectangleSolid 165 40)),
													Color white (Translate (-35) (-375) (Scale 0.15 0.15 (Text ("Time: "++show(timer world)))))
													]

-- |Print the Picture for the Pause Button.
printPauseButton :: Picture
printPauseButton = Pictures [
											Color violet (Translate (175) (-370) (rectangleSolid 70 40)),
											Color white (Translate (147) (-375) (Scale 0.15 0.15 (Text "Pause!")))
											]

-- |Print the Picture for the Save Button.
printSaveButton :: Picture
printSaveButton = Pictures [
													Color violet (Translate (-400) (-75) (rectangleSolid 60 50)),
													Color white (Translate (-422) (-72) (Scale 0.15 0.15 (Text "Save"))),
													Color white (Translate (-427) (-93) (Scale 0.15 0.15 (Text "Game")))
													]

-- |Print the Picture for the Load Button.
printLoadButton :: Picture
printLoadButton = Pictures [
													Color violet (Translate (-400) (-150) (rectangleSolid 60 50)),
													Color white (Translate (-422) (-148) (Scale 0.15 0.15 (Text "Load"))),
													Color white (Translate (-427) (-168) (Scale 0.15 0.15 (Text "Game")))
													]

-- |Print the Picture for the Undo Button.
printUndoButton :: Picture
printUndoButton  = Pictures [
											Color violet (Translate (-200) (-370) (rectangleSolid 60 40)),
											Color white (Translate (-224) (-375) (Scale 0.15 0.15 (Text "Undo!")))
											]

-- |Print the Picture for the Increase Board Size Button.
printIncreaseBoardSizeButton :: Picture
printIncreaseBoardSizeButton = Pictures [
																	Color violet (Translate (-400) (270) (rectangleSolid 60 30)),
																	Color white (Translate (-413) (265) (Scale 0.15 0.15 (Text "/\\")))
																	]

-- |Print the Picture for the Board Size Button.
printBoardSizeButton :: Picture
printBoardSizeButton = Pictures[
													Color violet (Translate (-400) (215) (rectangleSolid 60 55)),
													Color white (Translate (-425) (220) (Scale 0.15 0.15 (Text "Board"))),
													Color white (Translate (-418) (195) (Scale 0.15 0.15 (Text "Size")))
														]

-- |Print the Picture for the Decrease Board Size Button.
printDecreaseBoardSizeButton :: Picture
printDecreaseBoardSizeButton = Pictures [
																	Color violet (Translate (-400) (160) (rectangleSolid 60 30)),
																	Color white (Translate (-413) (155) (Scale 0.15 0.15 (Text "\\/")))
																	]

-- |Print the Picture for the Increase Line Size Button.
printIncreaseLineButton :: Picture
printIncreaseLineButton = Pictures [
																Color violet (Translate (-400) (110) (rectangleSolid 60 30)),
																Color white (Translate (-413) (105) (Scale 0.15 0.15 (Text "/\\")))
																]

-- |Print the Picture for the Line Size Button.
printLineSizeButton :: World -> Picture
printLineSizeButton world = Pictures[
																Color violet (Translate (-400) (50) (rectangleSolid 60 70)),
																Color white (Translate (-420) (60) (Scale 0.15 0.15 (Text "Line"))),
																Color white (Translate (-420) (40) (Scale 0.15 0.15 (Text "Size:"))),
																Color white (Translate (-410) (20) (Scale 0.15 0.15 (Text (show(target (board world))))))
																]

-- |Print the Picture for the Decrease Line Size Button.
printDecreaseLineButton :: Picture
printDecreaseLineButton = Pictures [
																Color violet (Translate (-400) (-10) (rectangleSolid 60 30)),
																Color white (Translate (-413) (-15) (Scale 0.15 0.15 (Text "\\/")))
																]

-- |Print the Picture for all the of the squares on the Board.
printBoard :: World -- ^The current world.
										-> Picture -- ^The board Picture.
printBoard w = Pictures [square x y (board w) w| x <- reverse(generateXList (board w)), y <- (generateList (board w))]

-- |Prints the Square and if there is a piece on the board, it prints that too.
square :: Float -- ^The x coordinate for the square.
								-> Float -- ^The y coordinate for the square.
								-> Board -- ^The current board.
								-> World -- ^The current world.
								-> Picture -- ^The Picture of the square.
square x y board world |pics world == True && pieceHere ((convertToColumnMultiColumn x board), (convertToColumnMultiRow y board)) board == True && getColour ((convertToColumnMultiColumn x board), (convertToColumnMultiRow y board)) board == Black = Pictures[
																																																																											(Translate x y  (Scale (getPictureSize board 1) (getPictureSize board 2) (squares world))),
																																																																											(Translate x y  (Scale (sel1(getCounterSize board 1)) (sel2(getCounterSize board 1)) (counterOne world)))
																																																																											--Color black (Translate x y (circleSolid (getCircleSize board)))
																																																																											]
								       |pics world == True && pieceHere ((convertToColumnMultiColumn x board), (convertToColumnMultiRow y board)) board == True && getColour ((convertToColumnMultiColumn x board), (convertToColumnMultiRow y board)) board == White = Pictures[
			 																																																																											(Translate x y  (Scale (getPictureSize board 1) (getPictureSize board 2) (squares world))),
																																																																														(Translate x y  (Scale (sel1(getCounterSize board 2)) (sel2(getCounterSize board 2)) (counterTwo world)))
			 																																																																											--Color white (Translate x y (circleSolid (getCircleSize board )))
			 																																																																											]
											 |pics world == True = (Translate x y  (Scale (getPictureSize board 1) (getPictureSize board 2) (squares world)))
											 |pieceHere ((convertToColumnMultiColumn x board), (convertToColumnMultiRow y board)) board == True && getColour ((convertToColumnMultiColumn x board), (convertToColumnMultiRow y board)) board == Black = Pictures[
																																																																											Color blue (Translate x y (rectangleSolid (getSquareSize board) (getSquareSize board))),
																																																																											Color black (Translate x y (circleSolid (getCircleSize board)))
																																																																											]
								       |pieceHere ((convertToColumnMultiColumn x board), (convertToColumnMultiRow y board)) board == True && getColour ((convertToColumnMultiColumn x board), (convertToColumnMultiRow y board)) board == White = Pictures[
			 																																																																											Color blue (Translate x y (rectangleSolid (getSquareSize board) (getSquareSize board))),
			 																																																																											Color white (Translate x y (circleSolid (getCircleSize board )))
			 																																																																											]
								       |otherwise = Color blue (Translate x y  (rectangleSolid (getSquareSize board) (getSquareSize board)))

-- |Getting the counter size via the size of the board.
getCounterSize :: Board -- ^The current board.
											-> Int -- ^Either 1 or 2 depending on which counter it is
											-> (Float,Float) -- ^The size of an individual circle.
getCounterSize board counter |counter == 1 && size board == 7 = (0.03,0.04)
														 |counter == 1 && size board == 8 = (0.03,0.036)
														 |counter == 1 && size board == 9 = (0.028,0.035)
														 |counter == 1 && size board == 10 = (0.026,0.03)
														 |counter == 1 && size board == 11 = (0.026,0.028)
														 |counter == 1 && size board == 12 = (0.026,0.026)
														 |counter == 1 && size board == 13 = (0.023,0.023)
														 |counter == 1 && size board == 14 = (0.022,0.02)
														 |counter == 1 && size board == 15 = (0.022,0.02)
														 |counter == 1 && size board == 16 = (0.022,0.02)
														 |counter == 1 && size board == 17 = (0.022,0.02)
														 |counter == 1 && size board == 18 = (0.022,0.02)
														 |counter == 1 && size board == 19 = (0.022,0.02)
														 |counter == 1 = (0.04,0.05)
														 |counter == 2 && size board == 7 = (0.09,0.09)
														 |counter == 2 && size board == 8 = (0.08,0.07)
													 	 |counter == 2 && size board == 9 = (0.08,0.07)
														 |counter == 2 && size board == 10 = (0.073,0.066)
														 |counter == 2 && size board == 11 = (0.073,0.065)
														 |counter == 2 && size board == 12 = (0.065,0.06)
														 |counter == 2 && size board == 13 = (0.06,0.052)
														 |counter == 2 && size board == 14 = (0.05,0.04)
														 |counter == 2 && size board == 15 = (0.05,0.04)
														 |counter == 2 && size board == 16 = (0.05,0.04)
														 |counter == 2 && size board == 17 = (0.05,0.04)
														 |counter == 2 && size board == 18 = (0.05,0.04)
														 |counter == 2 && size board == 19 = (0.05,0.04)
														 |otherwise = (0.1,0.11)


-- |Getting the square size via the size of the board.
getPictureSize :: Board -- ^The current board.
											-> Int -- ^Either 1 or 2 depending on x or y number
											-> Float -- ^The size of an individual square.
getPictureSize board digit |digit == 1 && size board == 7 = 0.06
													|digit == 1 && size board == 8 = 0.053
													|digit == 1 && size board == 9 = 0.048
													|digit == 1 && size board == 10 = 0.043
													|digit == 1 && size board == 11 = 0.04
													|digit == 1 && size board == 12 = 0.038
													|digit == 1 && size board == 13 = 0.03
													|digit == 1 && size board == 14 = 0.028
													|digit == 1 && size board == 15 = 0.028
													|digit == 1 && size board == 16 = 0.028
													|digit == 1 && size board == 17 = 0.028
													|digit == 1 && size board == 18 = 0.028
													|digit == 1 && size board == 19 = 0.028
													|digit == 1 = 0.07
													|digit == 2 && size board == 7 = 0.097
													|digit == 2 && size board == 8 = 0.08
													|digit == 2 && size board == 9 = 0.072
													|digit == 2 && size board == 10 = 0.068
													|digit == 2 && size board == 11 = 0.0625
													|digit == 2 && size board == 12 = 0.06
													|digit == 2 && size board == 13 = 0.05
													|digit == 2 && size board == 14 = 0.042
													|digit == 2 && size board == 15 = 0.042
													|digit == 2 && size board == 16 = 0.042
													|digit == 2 && size board == 17 = 0.042
													|digit == 2 && size board == 18 = 0.042
													|digit == 2 && size board == 19 = 0.042
													|otherwise = 0.11

-- |Getting the square size via the size of the board.
getSquareSize :: Board -- ^The current board.
											-> Float -- ^The size of an individual square.
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

-- |Getting the piece size via the size of the board.
getCircleSize :: Board -- ^The current board.
											-> Float -- ^The size of an individual circle.
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

-- |Getting the list of x coordinates for the biggest size of board, otherwise it just calls the bother fuction to get the rest of the coordinates.
generateXList :: Board -- ^The current board.
												-> [Float] -- ^The list of coordinates
generateXList board |size board == 19 = [345, 307, 269, 231, 193, 155, 117, 79, 41, 3, -35, -73, -111, -149, -187, -225, -262, -300, -338]
									  |otherwise = generateList board

-- |Getting the list of coordinates for the different sizes of board.
generateList :: Board -- ^The current board.
												-> [Float] -- ^The list of coordinates
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

-- |Converting the x coordinate to the column position.
convertToColumnMultiColumn :: Float -- ^The x coordinate.
																		-> Board -- ^The current board.
																		-> Int -- ^The column number to return.
convertToColumnMultiColumn x board |length (generateXList board) > 0 && x<((reverse(generateXList board)!!0)+((getSquareSize board)/2)) && x>((reverse(generateXList board)!!0)-((getSquareSize board)/2)) = 1
							               |length (generateXList board) > 1 && x<((reverse(generateXList board)!!1)+((getSquareSize board)/2)) && x>((reverse(generateXList board)!!1)-((getSquareSize board)/2)) = 2
							               |length (generateXList board) > 2 && x<((reverse(generateXList board)!!2)+((getSquareSize board)/2)) && x>((reverse(generateXList board)!!2)-((getSquareSize board)/2))  = 3
							               |length (generateXList board) > 3 && x<((reverse(generateXList board)!!3)+((getSquareSize board)/2)) && x>((reverse(generateXList board)!!3)-((getSquareSize board)/2))  = 4
							               |length (generateXList board) > 4 && x<((reverse(generateXList board)!!4)+((getSquareSize board)/2)) && x>((reverse(generateXList board)!!4)-((getSquareSize board)/2))  = 5
							               |length (generateXList board) > 5 && x<((reverse(generateXList board)!!5)+((getSquareSize board)/2)) && x>((reverse(generateXList board)!!5)-((getSquareSize board)/2))  = 6
														 |length (generateXList board) > 6 && x<((reverse(generateXList board)!!6)+((getSquareSize board)/2)) && x>((reverse(generateXList board)!!6)-((getSquareSize board)/2))  = 7
														 |length (generateXList board) > 7 && x<((reverse(generateXList board)!!7)+((getSquareSize board)/2)) && x>((reverse(generateXList board)!!7)-((getSquareSize board)/2))  = 8
														 |length (generateXList board) > 8 && x<((reverse(generateXList board)!!8)+((getSquareSize board)/2)) && x>((reverse(generateXList board)!!8)-((getSquareSize board)/2))  = 9
														 |length (generateXList board) > 9 && x<((reverse(generateXList board)!!9)+((getSquareSize board)/2)) && x>((reverse(generateXList board)!!9)-((getSquareSize board)/2))  = 10
														 |length (generateXList board) > 10 && x<((reverse(generateXList board)!!10)+((getSquareSize board)/2)) && x>((reverse(generateXList board)!!10)-((getSquareSize board)/2))  = 11
														 |length (generateXList board) > 11 && x<((reverse(generateXList board)!!11)+((getSquareSize board)/2)) && x>((reverse(generateXList board)!!11)-((getSquareSize board)/2))  = 12
														 |length (generateXList board) > 12 && x<((reverse(generateXList board)!!12)+((getSquareSize board)/2)) && x>((reverse(generateXList board)!!12)-((getSquareSize board)/2))  = 13
														 |length (generateXList board) > 13 && x<((reverse(generateXList board)!!13)+((getSquareSize board)/2)) && x>((reverse(generateXList board)!!13)-((getSquareSize board)/2))  = 14
														 |length (generateXList board) > 14 && x<((reverse(generateXList board)!!14)+((getSquareSize board)/2)) && x>((reverse(generateXList board)!!14)-((getSquareSize board)/2))  = 15
														 |length (generateXList board) > 15 && x<((reverse(generateXList board)!!15)+((getSquareSize board)/2)) && x>((reverse(generateXList board)!!15)-((getSquareSize board)/2))  = 16
														 |length (generateXList board) > 16 && x<((reverse(generateXList board)!!16)+((getSquareSize board)/2)) && x>((reverse(generateXList board)!!16)-((getSquareSize board)/2))  = 17
														 |length (generateXList board) > 17 && x<((reverse(generateXList board)!!17)+((getSquareSize board)/2)) && x>((reverse(generateXList board)!!17)-((getSquareSize board)/2))  = 18
														 |length (generateXList board) > 18 && x<((reverse(generateXList board)!!18)+((getSquareSize board)/2)) && x>((reverse(generateXList board)!!18)-((getSquareSize board)/2))  = 19
							               |otherwise = 0

-- |Converting the y coordinate to the row position.
convertToColumnMultiRow :: Float -- ^The y coordinate.
																		-> Board -- ^The current board.
																		-> Int -- ^The row number to return.
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

-- |Returns whether or not there is a piece on the position given.
pieceHere :: Position -- ^The position to be checked.
											-> Board -- ^The current board.
											-> Bool -- ^True or False for whether there is a psoition on the board.
pieceHere pos board = elem pos (map (\ posi -> sel1  posi) piecesOnBoard)
		      where piecesOnBoard = pieces board

-- |Get the colour of of the piece on a position.
getColour :: Position -- ^The position to check.
											-> Board -- ^The current board.
											-> Col -- ^The colour of the piece on that position.
getColour pos board = sel2 (head[x | x <- pieces board, sel1 x == pos])
