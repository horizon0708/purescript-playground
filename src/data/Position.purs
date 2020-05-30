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

-- this feels like a monad? need to lift and unlift
newtype RelativePosition = RelativePosition (Tuple Int Int)
derive newtype instance eqRelPos :: Eq RelativePosition
derive newtype instance showRelPos :: Show RelativePosition
derive newtype instance ordRelPos :: Ord RelativePosition

toRelative :: Position -> Position -> RelativePosition
toRelative offset pos = RelativePosition (pos - offset)

toAbsolute :: Position -> RelativePosition -> Position
toAbsolute offset (RelativePosition pos) = pos + offset

toTuple :: RelativePosition -> Tuple Int Int
toTuple (RelativePosition pos) = pos

inRange :: Int -> Boolean
inRange n = n > 0 && n < 9

inBound :: Tuple Int Int -> Boolean
inBound (Tuple x y) = inRange x && inRange y   

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
filterByDirection direction = filter toPredicate' where
    toPredicate' :: RelativePosition -> Boolean
    toPredicate' pos = toPredicate direction (toTuple pos)  

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
closestOtherPiece arr = head $ sortBy sortByDistance' arr 
  where sortByDistance' a b = sortByDistance (toTuple a) (toTuple b)

test = [ Tuple 2 2, Tuple 4 4 , Tuple (-3) (-3) ]
testBoard = [ Tuple 2 2 ]
testBoard2 = []

lineMovement ::Array(RelativePosition) ->Int -> MovementDirection ->  Array (RelativePosition)
lineMovement o n direction = do
  x <- 0 .. (n * gx)
  y <- 0 .. (n * gy)
  guard $ (getDistance (Tuple x y)) < distance && toPredicate direction (Tuple x y)
  pure (RelativePosition (Tuple x y)) where
    Tuple gx gy = toNormalized direction 
    distance = case closestOtherPiece (filterByDirection direction o) of
      Just t-> getDistance (toTuple t)
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

