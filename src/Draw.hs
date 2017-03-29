module Draw(drawWorld) where

import Graphics.Gloss
import Board

-- Given a world state, return a Picture which will render the world state.
-- Currently just draws a single blue circle as a placeholder.
--
-- This will need to extract the Board from the world state and draw it
-- as a grid plus pieces.
drawWorld :: World -> Picture
--drawWorld w = Color blue $ Circle 50

 --drawWorld ww = Color blue $ Circle 150

drawWorld w = Pictures [
                Color white (Circle 5)
                --Color yellow (Translate (-250) 0 (Circle))
                ]
