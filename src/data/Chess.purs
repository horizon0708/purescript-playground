module Data.Chess where

import Prelude
import Data.Array
import Data.Tuple

type Piece = { position :: Position, side :: Side, type :: Type }
type Position = Tuple Int Int 

-- show instance

data Side = White | Black
data Type = King | Queen | Rook | Knight | Bishop | Pawn 

yStartPosition :: Side -> Type -> Int
yStartPosition White Pawn = 1
yStartPosition Black Pawn = 6
yStartPosition White _ = 0
yStartPosition Black _ = 7

setupPawns :: Side -> Array (Piece)
setupPawns side = do
    x <- 0 .. 8
    y <- [yStartPosition side Pawn] 
    pure { position: Tuple x y, side: side, type: Pawn } 

setupRow :: Side -> Array (Type) -> Array (Piece)
setupRow side types = do
    x <- 0 .. 8 
    y <- [yStartPosition side Knight]
    t <- types 
    pure { position: Tuple x y, side: side, type: t }