module Data.Position where

import Data.Array
import Data.Eq
import Data.Int
import Data.Maybe
import Data.Tuple
import Prelude

import Control.MonadZero (guard)
import Data.Newtype (overF)
import Math (abs)

type Position = Tuple Int Int  
type RelativePosition = Tuple Int Int
type Piece = { position :: Position }


inRange :: Int -> Boolean
inRange n = n > 0 && n < 9

inBound :: Tuple Int Int -> Boolean
inBound (Tuple x y) = inRange x && inRange y   

-- given a piece
-- a piece can only move to a certain square
data ChessPiece = 
    Knight Piece |
    Queen Piece |
    Rook Piece

data ChessDirection =
    UpDiagonal |
    DownDiagonal |
    Horizontal | 
    Vertical 

data MovementDirection = 
  Up |
  UpRight |
  Right |
  DownRight |
  Down | 
  DownLeft |
  Left |
  UpLeft 

type Range = Int

-- to relative pos
toRelativePositions :: Tuple Int Int -> Array(Tuple Int Int) -> Array (Tuple Int Int)
toRelativePositions offset = map (\origin -> origin - offset)

type PiecesInDirection = Array (RelativePosition)

toPredicate :: MovementDirection -> Tuple Int Int -> Boolean
toPredicate Up (Tuple x y) = x == 0 && y > 0
toPredicate UpRight (Tuple x y) = y == x && x > 0 && y > 0
toPredicate Right (Tuple x y) =  y == 0 && x > 0
toPredicate DownRight (Tuple x y) = y == (-x) && x > 0 && y < 0
toPredicate Down (Tuple x y) = x == 0 && y < 0
toPredicate DownLeft (Tuple x y) = y == x && x < 0 && y < 0
toPredicate Left (Tuple x y) = y == 0 && x < 0
toPredicate UpLeft (Tuple x y) = y == (-x) && x < 0 && y > 0

toNormalized :: MovementDirection -> Tuple Int Int
toNormalized Up = Tuple 0 1
toNormalized UpRight = Tuple 1 1 
toNormalized Right = Tuple 1 0
toNormalized DownRight = Tuple 1 (-1)
toNormalized Down = Tuple 0 (-1)
toNormalized DownLeft = Tuple (-1) (-1)
toNormalized Left = Tuple (-1) 0
toNormalized UpLeft = Tuple (-1) 1

filterByDirection :: MovementDirection -> Array (RelativePosition) -> PiecesInDirection
filterByDirection direction = filter (toPredicate direction)

absInt :: Int -> Int
absInt n | n < 0 = n * -1
absInt n = n

absTuple :: Tuple Int Int -> Tuple Int Int
absTuple (Tuple x y) = Tuple (absInt x) (absInt y)

-- cheating because we know |x| = |y| if x /= 0 and y /= 0
getDistance :: Tuple Int Int -> Int 
getDistance (Tuple 0 n) = n
getDistance (Tuple n 0) = n
getDistance (Tuple 0 0) = 0
getDistance (Tuple x y) = x

sortByDistance :: Tuple Int Int -> Tuple Int Int -> Ordering
sortByDistance a b | absTuple a == absTuple b = EQ
sortByDistance a b | absTuple a > absTuple b = GT
sortByDistance a b | absTuple a < absTuple b = LT
sortByDistance _ _ = EQ

closestOtherPiece :: PiecesInDirection -> Maybe RelativePosition
closestOtherPiece arr = head $ sortBy sortByDistance arr 

test = [ Tuple 2 2, Tuple 4 4 , Tuple (-3) (-3) ]
testBoard = [ Tuple 2 2 ]
testBoard2 = []

testFn :: RelativePosition -> RelativePosition
testFn r = r

lineMovement ::Array(RelativePosition) ->Int -> MovementDirection ->  Array (RelativePosition)
lineMovement o n direction = do
  x <- 0 .. (n * gx)
  y <- 0 .. (n * gy)
  guard $ (getDistance (Tuple x y)) < distance && toPredicate direction (Tuple x y)
  pure (Tuple x y) where
    Tuple gx gy = toNormalized direction 
    distance = case closestOtherPiece (filterByDirection direction o) of
      Just t-> getDistance t
      _ -> n 

combineLineMovements :: Int ->  Array(MovementDirection)-> Array(RelativePosition) -> Array(RelativePosition)
combineLineMovements n dirs o = concat $ map (\x -> movement x) dirs where
  movement = lineMovement o n

kingMovement :: Array(RelativePosition) -> Array(RelativePosition)
kingMovement = combineLineMovements 1 [
  Up, UpRight, Right, DownRight, Down, DownLeft, Left, UpLeft
]

queenMovement :: Array(RelativePosition) -> Array(RelativePosition)
queenMovement = combineLineMovements 8 [
  Up, UpRight, Right, DownRight, Down, DownLeft, Left, UpLeft
]

rookMovement :: Array(RelativePosition) -> Array(RelativePosition)
rookMovement = combineLineMovements 8 [
  Up , Right, Down, Left
] 

bishopMovement :: Array(RelativePosition) -> Array(RelativePosition)
bishopMovement = combineLineMovements 8 [
  UpRight , DownRight , DownLeft, UpLeft 
]

-- how do i tell it to stop once it finds enemy?

-- lineMovement :: MovementDirection -> Int -> Array(RelativePosition) -> Array (RelativePosition) 
-- lineMovement Up range other = do
--   x <- [0..range]
--   y <- [0..range]
--   pure (Tuple x y)

-- lineMovement gradient range other = do 
--     x <- [-xMin .. xMax]
--     y <- [-range .. range]
--     pure (Tuple x y)
--     where 
--       o = filterByGradient gradient other
--       xArr = cons range (map fst o)
--       xMax = max xArr
--       xMin = min xArr 

-- pieceMovement :: ChessPiece -> Array(Tuple Int Int) -> Array (Tuple Int Int)
-- pieceMovement (Knight { position: Tuple x y}) _ = do
--   x2 <- [ x + 1, x + 2, x - 1, x -2]
--   y2 <- [ y + 1, y + 2, y - 1, y -2]
--   guard $ x2 /= y2
--   guard $ x2 * -1 /= y2
--   pure (Tuple x2 y2)
-- pieceMovement (Queen { position: Tuple x y}) board = do
--   x2 <- (map \n -> n + x) [-7..7]
--   y2 <- (map \n -> n + y) [-7..7]
--   pure (Tuple x2 y2)

-- if the square is not within the piece Movement ...

-- filterOutOfBound :: Array (Tuple Int Int) -> Array(Tuple Int Int)  
-- filterOutOfBound = filter inBound




-- isInMoveSet :: ChessPiece -> Position -> Maybe ChessPiece
-- isInMoveSet p = do
--   _ <- find (\x -> x == p.position) (pieceMovement p)
--   pure (Just p)


c1 :: ChessPiece
c1 = Knight { position : Tuple 0 0}

showChessPiece :: ChessPiece -> String
showChessPiece (Knight { position: (Tuple x y)}) = "Knight at " <> show x <> ", " <> show y <> "."
showChessPiece (Queen { position: (Tuple x y)}) = "Queen at " <> show x <> ", " <> show y <> "."
showChessPiece (Rook { position: (Tuple x y)}) = "Rook at " <> show x <> ", " <> show y <> "."

instance showChessPieceInstance :: Show ChessPiece where
  show = showChessPiece



-- movePiece :: ChessPiece -> Position -> Maybe ChessPiece
-- movePiece (Knight piece) position = do


--   pure $ Knight piece




-- the piece can only go to designated squares
-- the piece cannot move past other pieces (unless its a knight)
-- the piece cannot move to a square occupied by a friendly piece   
-- the move must not must not be put king into check


