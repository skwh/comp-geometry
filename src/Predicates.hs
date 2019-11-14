module
  Predicates 
  (
    dotProduct
  , scalarProduct
  , crossProduct
  , turn
  , above
  )
where

  import Primitives

  dotProduct :: (Num a) => Vector a -> Vector a -> a
  dotProduct (Vector ux uy) (Vector vx vy) = ux * vx + uy * vy

  scalarProduct :: (Num a) => Vector a -> a -> Vector a
  scalarProduct (Vector x y) t = Vector (t*x) (t*y)

  crossProduct :: (Num a) => Vector a -> Vector a -> a
  crossProduct (Vector ux uy) (Vector vx vy) = (ux*vy) - (vx*uy)

  -- turn p q r = p -> q -> r is a Left Turn
  --              p -> q -> r is a Right Turn
  --              p -> q -> r are collinear (No Turn)
  turn :: (Num a, Ord a) => Point a -> Point a -> Point a -> TurnType
  turn (Point xp yp) (Point xq yq) (Point xr yr)
    | cross < 0 = TurnRight
    | cross > 0 = TurnLeft
    | otherwise = NoTurn
    where qp = Vector (xq-xp) (yq-yp)
          rp = Vector (xr-xp) (yr-yp)
          cross = crossProduct qp rp

  boundingOverlap :: (Ord a) => BoundingBox a -> BoundingBox a -> Bool
  boundingOverlap _ _ = undefined

  above :: (Ord a) => Point a -> Point a -> Bool
  above (Point _ ny) (Point _ qy) = if qy > ny then True else False

