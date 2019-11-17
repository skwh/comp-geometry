module
  Predicates 
  (
    dotProduct
  , scalarProduct
  , crossProduct
  , turn
  , above
  , orient
  , intersects
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
  boundingOverlap (BoundingBox (Point x1 y1) (Point x2 y2)) (BoundingBox (Point x3 y3) (Point x4 y4))
    | (x2 >= x3 && x4 >= x1 && y2 >= y3 && y4 >= y1) = True
    | otherwise                                      = False

  above :: (Ord a) => Point a -> Point a -> Bool
  above (Point _ ny) (Point _ qy) = if qy > ny then True else False

  orient :: (Num a, Ord a) => Point a -> Point a -> Point a -> a
  orient (Point x0 y0) (Point x1 y1) (Point x2 y2) = (x1 - x0) * (y2 - y0) - (x2 - x0) * (y1 - y0)

  straddles :: (Num a, Ord a) => LineSegment a -> LineSegment a -> Bool
  straddles (LineSegment p1 p2) (LineSegment p3 p4)
    | orient p1 p2 p3 * orient p1 p2 p4 <= 0 = True
    | otherwise                              = False

  intersects :: (Num a, Ord a) => LineSegment a -> LineSegment a -> Bool
  intersects (LineSegment p1 p2) (LineSegment p3 p4) = boundingOverlap (BoundingBox p1 p2) (BoundingBox p3 p4) 
                                                    && straddles (LineSegment p1 p2) (LineSegment p3 p4) 
                                                    && straddles (LineSegment p3 p4) (LineSegment p1 p2)