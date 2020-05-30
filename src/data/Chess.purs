module Data.Chess where

import Data.Position
import Data.Array
import Data.Maybe
import Data.Tuple
import Prelude
import Control.Monad.State

import Data.Position (RelativePosition(..))

type Piece = { position :: Position, side :: Side, type :: Type }
-- show instance

newtype Game = Game {
    board :: Array (Piece),
    selectedSqaure :: Position,
    movableSquares :: Array (Position),
    turn :: Int
}

data Side = White | Black
data Type = King | Queen | Rook | Knight | Bishop | Pawn 
instance showType :: Show Type where
    show King =  "King"
    show Queen = "Queen"
    show Rook = "Rook"
    show Knight = "Knight"
    show Bishop = "Bishop"
    show Pawn = "Pawn"

instance showSide :: Show Side where
    show White = "White"
    show Black = "Black"

yStartPosition :: Side -> Type -> Int
yStartPosition White Pawn = 1
yStartPosition Black Pawn = 5
yStartPosition White _ = 0
yStartPosition Black _ = 6

setupPawns :: Side -> Array (Piece)
setupPawns side = do
    x <- 0 .. 7
    y <- [yStartPosition side Pawn] 
    pure { position: Tuple x y, side: side, type: Pawn } 

setupRow :: Array (Type) -> Side -> Array (Piece)
setupRow types side = do
    x <- 0 .. 7
    y <- [yStartPosition side Knight]
    pure { position: Tuple x y, side: side, type: fallbackToPawn types x }

fallbackToPawn :: Array (Type) -> Int -> Type
fallbackToPawn arr ind = case arr !! ind of
    Just t -> t
    Nothing -> Pawn 
    
setupBackrow :: Side -> Array(Piece)
setupBackrow = setupRow [ Rook, Knight, Bishop, Queen, King, Bishop, Knight, Rook ]

-- select the sqaure
-- find the Piece in that square
-- return possible movesets


-- is it worth changing this into ReaderT monad?
getOtherPositions :: Array(Piece) -> Position ->  Array (Position)
getOtherPositions board pos = case getPieceOnSquare board pos of
    Just _ -> toAbsolutePositions pos $ getRelativePositions pos board
    Nothing -> []

getPieceOnSquare :: Array(Piece) -> Position -> Maybe Piece
getPieceOnSquare pieces pos = find (\p -> p.position == pos) pieces

getRelativePositions :: Position -> Array(Piece) ->  Array(RelativePosition)
getRelativePositions offset = map toRelative' where
  toRelative' { position: pos } = toRelative offset pos 

toAbsolutePositions :: Position -> Array(RelativePosition) -> Array(Position)
toAbsolutePositions offset = map (toAbsolute offset)

-- getPossibleMovements:: Array(Piece) -> Position  -> Array(Position)

type GameState = State Game

relativePositions :: GameState (Array RelativePosition) 
relativePositions = do
  Game game <- get
  pure (map (toRelative' game)) where
    toRelative' { selectedSquare: sq, board: b} = toRelative sq (map \x -> x.position b)

-- toAbsolutePositions :: GameState (Array (Position))
-- toAbsolutePositions = do
--   Game game <- get
--   pure (map (toAbsolute game.selectedSqaure) game.board)


getMovementOf :: Type -> Array(RelativePosition) -> Array(RelativePosition)
getMovementOf Queen = queenMovement
getMovementOf King = kingMovement
getMovementOf Rook = rookMovement
getMovementOf Bishop = bishopMovement
getMovementOf _ = \_ -> []

-- send square and target square
-- validate the movement
-- change the boardstate accordingly

