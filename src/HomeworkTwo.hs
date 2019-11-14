module 
  HomeworkTwo
  (
    
  )   
where

  import Primitives
  import Predicates

  isLeftTangentPoint :: (Num a, Ord a) => Point a -> Point a -> Point a -> Point a -> Bool
  isLeftTangentPoint prevPoint curPoint nxtPoint testPoint
    | bothTurn  = True
    | otherwise = False
    where firstTurn = turn testPoint curPoint prevPoint
          secondTurn = turn testPoint curPoint nxtPoint
          bothTurn = firstTurn == TurnLeft && secondTurn == TurnLeft

  leftTangent :: (Num a, Ord a) => Polygon a -> Point a -> LineSegment a 
  leftTangent (Polygon ps) p = findTangentPoint cps p
    where cps = cycle ps
          findTangentPoint (va:vb:vc:vs) extPt | isLeftTangentPoint va vb vc extPt = LineSegment vb extPt
                                               | otherwise = findTangentPoint (vb:vc:vs) extPt