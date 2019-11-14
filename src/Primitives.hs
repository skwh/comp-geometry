module
  Primitives 
  (
    Point (..)
  , Vector (..)
  , Line (..)
  , LineSegment (..)
  , Polygon (..)
  , Circle (..)
  , Triangle (..)
  , TurnType (..)
  , BoundingBox (..)
  )
where

  data TurnType = TurnLeft | TurnRight | NoTurn deriving (Eq, Show)

  data Point a = Point { px :: a, py :: a } deriving (Eq, Ord, Show)
  data Vector a = Vector { vx :: a, vy :: a } deriving (Eq, Show)
  data Line x = Line { a :: x , b :: x, c :: x } deriving (Eq, Show)
  data LineSegment x = LineSegment { p1 :: Point x , p2 :: Point x } deriving (Eq, Show)
  data Polygon a = Polygon { pvs :: [Point a] } deriving (Eq, Show)
  data Circle a = Circle { center :: Point a , radius :: a } deriving (Eq, Show)
  data Triangle a = Triangle { tvs :: (Point a, Point a, Point a) } deriving (Eq, Show)
  data BoundingBox a = BoundingBox { pl :: Point a , pr :: Point a } deriving (Eq, Show) 