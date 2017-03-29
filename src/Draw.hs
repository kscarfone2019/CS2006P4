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
                Color yellow (Translate (-250) 150 (rectangleSolid 75 75)),
                Color yellow (Translate (-250) 50 (rectangleSolid 75 75)),
                Color yellow (Translate (-250) (-50) (rectangleSolid 75 75)),
		Color yellow (Translate (-250) 250 (rectangleSolid 75 75)),
                Color yellow (Translate (-250) (-150) (rectangleSolid 75 75)),
                Color yellow (Translate (-250) (-250) (rectangleSolid 75 75)),

		--column 2
		Color yellow (Translate (-150) 150 (rectangleSolid 75 75)),
		Color yellow (Translate (-150) 50 (rectangleSolid 75 75)),
		Color yellow (Translate (-150) (-50) (rectangleSolid 75 75)),
		Color yellow (Translate (-150) 250 (rectangleSolid 75 75)),
		Color yellow (Translate (-150) (-150) (rectangleSolid 75 75)),
		Color yellow (Translate (-150) (-250) (rectangleSolid 75 75)),

		--column 3
		Color yellow (Translate (-50) 150 (rectangleSolid 75 75)),
		Color yellow (Translate (-50) 50 (rectangleSolid 75 75)),
		Color yellow (Translate (-50) (-50) (rectangleSolid 75 75)),
		Color yellow (Translate (-50) 250 (rectangleSolid 75 75)),
		Color yellow (Translate (-50) (-150) (rectangleSolid 75 75)),
		Color yellow (Translate (-50) (-250) (rectangleSolid 75 75)),

		--column 4
		Color yellow (Translate 50 150 (rectangleSolid 75 75)),
		Color yellow (Translate 50 50 (rectangleSolid 75 75)),
		Color yellow (Translate 50 (-50) (rectangleSolid 75 75)),
		Color yellow (Translate 50 250 (rectangleSolid 75 75)),
		Color yellow (Translate 50 (-150) (rectangleSolid 75 75)),
		Color yellow (Translate 50 (-250) (rectangleSolid 75 75)),

		--column 5
		Color yellow (Translate 150 150 (rectangleSolid 75 75)),
		Color yellow (Translate 150 50 (rectangleSolid 75 75)),
		Color yellow (Translate 150 (-50) (rectangleSolid 75 75)),
		Color yellow (Translate 150 250 (rectangleSolid 75 75)),
		Color yellow (Translate 150 (-150) (rectangleSolid 75 75)),
		Color yellow (Translate 150 (-250) (rectangleSolid 75 75)),

		--column 6
		Color yellow (Translate 250 150 (rectangleSolid 75 75)),
		Color yellow (Translate 250 50 (rectangleSolid 75 75)),
		Color yellow (Translate 250 (-50) (rectangleSolid 75 75)),
		Color yellow (Translate 250 250 (rectangleSolid 75 75)),
		Color yellow (Translate 250 (-150) (rectangleSolid 75 75)),
		Color yellow (Translate 250 (-250) (rectangleSolid 75 75))
                ]

pieceHere :: Position -> Board -> Bool

pieceHere pos board = elem pos (map (\ posi -> sel1 board) pieces board)

--getColour :: Position -> Board -> Col
--getColor = undefined

