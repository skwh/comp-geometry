module
  PointLocation
  (

  )
where
  import Primitives
  import Predicates

  import Data.Maybe
  import Data.IntMap.Lazy (IntMap)
  import qualified Data.IntMap.Lazy as IntMap

  data HorizontalDirection = DirLeft | DirRight deriving (Eq, Show)
  data VerticalDirection   = DirUp   | DirDown  deriving (Eq, Show)

  data Trapezoid a = Trapezoid { trapezoidId :: Int
                               , top :: LineSegment a
                               , bottom :: LineSegment a
                               , leftP :: Point a
                               , rightP :: Point a
                               , upperLeftNeighbor :: Maybe IntMap.Key
                               , lowerLeftNeighbor :: Maybe IntMap.Key
                               , upperRightNeighbor :: Maybe IntMap.Key
                               , lowerRightNeighbor :: Maybe IntMap.Key
                               } deriving (Eq, Show)
  
  type TrapezoidMap a = IntMap (Trapezoid a)

  getNewestMaxId :: TrapezoidMap a -> IntMap.Key
  getNewestMaxId tm = (+1) . head . reverse $ IntMap.keys tm

  getNewIds :: TrapezoidMap a -> Int -> [IntMap.Key]
  getNewIds tm count = let max = getNewestMaxId tm
                        in map (+max) [1..count]

  emptyTrapezoidMap :: Point a -> Point a -> TrapezoidMap a
  emptyTrapezoidMap (Point minx miny) (Point maxx maxy) = IntMap.singleton 0 Trapezoid { trapezoidId = 0
                                                                                       , top = LineSegment (Point minx maxy) (Point maxx maxy)
                                                                                       , bottom = LineSegment (Point minx miny) (Point maxx miny)
                                                                                       , leftP = Point minx miny
                                                                                       , rightP = Point maxx maxy
                                                                                       , upperLeftNeighbor = Nothing
                                                                                       , lowerLeftNeighbor = Nothing 
                                                                                       , upperRightNeighbor = Nothing
                                                                                       , lowerRightNeighbor = Nothing
                                                                                       }

  data SearchStructure a = XNode (Point a) (SearchStructure a) (SearchStructure a)       |
                           YNode (LineSegment a) (SearchStructure a) (SearchStructure a) |
                           LeafNode IntMap.Key                                           deriving (Eq, Show)

  emptySearchStructure :: TrapezoidMap a -> SearchStructure a
  emptySearchStructure map = LeafNode 0

  type RefinementPair a = (TrapezoidMap a, SearchStructure a)

  emptyRefinementPair :: Point a -> Point a -> RefinementPair a
  emptyRefinementPair min max = let tm = emptyTrapezoidMap min max
                                in  (tm, emptySearchStructure tm)

  rebuildRefinementPair :: (Ord a, Num a) => RefinementPair a -> LineSegment a -> RefinementPair a
  rebuildRefinementPair rp ls = processTrapezoids rp (followSegment rp ls) ls

  followSegment :: (Ord a, Num a) => RefinementPair a -> LineSegment a -> [Trapezoid a]
  followSegment (tm, ss) (LineSegment p q) = followSegment' [tm IntMap.! queryStructure ss p] q
    where followSegment' []     q = undefined -- this should never happen
          followSegment' (t:ts) q = case xNodeTest (rightP t) q
                                      of DirRight -> if yNodeTest (bottom t) q == DirUp
                                                     then t : followSegment' ((tm IntMap.! (fromJust (lowerRightNeighbor t))) : ts) q
                                                     else t : followSegment' ((tm IntMap.! (fromJust (upperRightNeighbor t))) : ts) q
                                         DirLeft  -> (t:ts)

  processTrapezoids :: (Ord a, Num a) => RefinementPair a -> [Trapezoid a] -> LineSegment a -> RefinementPair a
  processTrapezoids p [t] ls = bcts p t ls
  processTrapezoids (tm, ss) (t:ts) (LineSegment p q) = let tm' = IntMap.delete (trapezoidId t) . IntMap.insert (trapezoidId newTrapezoidLeft) newTrapezoidLeft .
                                                                                                  IntMap.insert (trapezoidId newTrapezoidTop) newTrapezoidTop   .
                                                                                                  IntMap.insert (trapezoidId newTrapezoidBottom) newTrapezoidBottom $ tm
                                                            ss' = (XNode p (LeafNode $ trapezoidId newTrapezoidLeft) 
                                                                           (YNode (LineSegment p q) 
                                                                                  (LeafNode $ trapezoidId newTrapezoidTop) (LeafNode $ trapezoidId newTrapezoidBottom)))
                                                        in processLeftToRight (tm', appendAtTrapezoid ss ss' p) (newTrapezoidTop, newTrapezoidBottom) ts
    where newIds = getNewIds tm 3
          newTrapezoidLeft = Trapezoid { trapezoidId = head newIds
                                       , top = top t
                                       , bottom = bottom t
                                       , leftP = leftP t
                                       , rightP = p
                                       , upperLeftNeighbor = upperLeftNeighbor t
                                       , lowerLeftNeighbor = lowerLeftNeighbor t
                                       , upperRightNeighbor = Just $ newIds !! 1
                                       , lowerRightNeighbor = Just $ newIds !! 2
                                       }
          newTrapezoidTop = Trapezoid { trapezoidId = newIds !! 1
                                      , top = top t
                                      , bottom = LineSegment p q
                                      , leftP = p
                                      , rightP = rightP t
                                      , upperLeftNeighbor = Just $ head newIds
                                      , lowerLeftNeighbor = Nothing
                                      , upperRightNeighbor = upperRightNeighbor t
                                      , lowerRightNeighbor = Nothing 
                                      }
          newTrapezoidBottom = Trapezoid { trapezoidId = newIds !! 2
                                         , top = LineSegment p q
                                         , bottom = bottom t
                                         , leftP = p
                                         , rightP = rightP t
                                         , upperLeftNeighbor = Nothing
                                         , lowerLeftNeighbor = Just $ head newIds
                                         , upperRightNeighbor = Nothing
                                         , lowerRightNeighbor = lowerRightNeighbor t
                                         }

  processLeftToRight :: RefinementPair a -> (Trapezoid a, Trapezoid a) -> [Trapezoid a] -> RefinementPair a
  processLeftToRight (tm, ss) (top, bottom) [t]    = undefined 
  processLeftToRight (tm, ss) (top, bottom) (t:ts) = undefined

  bcts :: (Ord a, Num a) => RefinementPair a -> Trapezoid a -> LineSegment a -> RefinementPair a
  bcts (tm, ss) t (LineSegment p q) = let ss' = (XNode p (LeafNode $ trapezoidId newTrapezoidA) 
                                                          (XNode q (YNode (LineSegment p q) 
                                                                          (LeafNode $ trapezoidId newTrapezoidC) (LeafNode $ trapezoidId newTrapezoidD)) 
                                                          (LeafNode $ trapezoidId newTrapezoidB)))
                                          tm' = IntMap.delete (trapezoidId t) . IntMap.insert (trapezoidId newTrapezoidA) newTrapezoidA . 
                                                                                IntMap.insert (trapezoidId newTrapezoidB) newTrapezoidB . 
                                                                                IntMap.insert (trapezoidId newTrapezoidC) newTrapezoidC . 
                                                                                IntMap.insert (trapezoidId newTrapezoidD) newTrapezoidD $ tm
                                      in (tm', appendAtTrapezoid ss ss' p)
    where newIds = getNewIds tm 4
          newTrapezoidA = Trapezoid { trapezoidId = head newIds
                                    , top = top t
                                    , bottom = bottom t
                                    , leftP = leftP t
                                    , rightP = p
                                    , upperLeftNeighbor = upperLeftNeighbor t
                                    , lowerLeftNeighbor = lowerLeftNeighbor t
                                    , upperRightNeighbor = Just $ newIds !! 2
                                    , lowerRightNeighbor = Just $ newIds !! 3
                                    }
          newTrapezoidB = Trapezoid { trapezoidId = newIds !! 1
                                    , top = top t
                                    , bottom = bottom t
                                    , leftP = q
                                    , rightP = rightP t
                                    , upperLeftNeighbor = Just $ newIds !! 2
                                    , lowerLeftNeighbor = Just $ newIds !! 3
                                    , upperRightNeighbor = upperRightNeighbor t
                                    , lowerRightNeighbor = lowerRightNeighbor t
                                    }
          newTrapezoidC = Trapezoid { trapezoidId = newIds !! 2
                                    , top = top t
                                    , bottom = LineSegment p q
                                    , leftP = p
                                    , rightP = q
                                    , upperLeftNeighbor = Just $ head newIds
                                    , lowerLeftNeighbor = Nothing
                                    , upperRightNeighbor = Just $ newIds !! 1
                                    , lowerRightNeighbor = Nothing
                                    }
          newTrapezoidD = Trapezoid { trapezoidId = newIds !! 3
                                    , top = LineSegment p q
                                    , bottom = bottom t
                                    , leftP = p
                                    , rightP = q
                                    , upperLeftNeighbor = Nothing
                                    , lowerLeftNeighbor = Just $ head newIds
                                    , upperRightNeighbor = Nothing
                                    , lowerRightNeighbor = Just $ newIds !! 1
                                    }

  -- Is the query point to the left or to the right of the given point?
  xNodeTest :: (Ord a) => Point a -> Point a -> HorizontalDirection
  xNodeTest (Point nx ny) (Point qx qy)
    | qx < nx = DirLeft
    | qx > nx = DirRight
    | qy < ny = DirLeft
    | otherwise = DirRight

  -- Is the query point above or below the given line segment?
  yNodeTest :: (Ord a, Num a) => LineSegment a -> Point a -> VerticalDirection
  yNodeTest (LineSegment p1 p2) q
    | isLeftTurn = DirUp
    | otherwise  = DirDown
    where isLeftTurn = turn p1 p2 q == TurnLeft

  -- replace a leaf node with a subtree
  appendAtTrapezoid :: (Ord a, Num a) => SearchStructure a -> SearchStructure a -> Point a -> SearchStructure a
  appendAtTrapezoid (XNode p ssl ssr) nt q = if xNodeTest p q == DirLeft
                                             then (XNode p (appendAtTrapezoid ssl nt q) ssr)
                                             else (XNode p ssl (appendAtTrapezoid ssr nt q))
  appendAtTrapezoid (YNode l ssa ssb) nt q = if yNodeTest l q == DirUp
                                             then (YNode l (appendAtTrapezoid ssa nt q) ssb)
                                             else (YNode l ssa (appendAtTrapezoid ssb nt q))
  appendAtTrapezoid (LeafNode t)      nt _ = nt 

  -- query the structure given a point
  queryStructure :: (Ord a, Num a) => SearchStructure a -> Point a -> IntMap.Key
  queryStructure ss q = case ss
                        of   (XNode p ssl ssr) -> if xNodeTest p q == DirLeft 
                                                  then queryStructure ssl q 
                                                  else queryStructure ssr q
                             (YNode l ssa ssb) -> if yNodeTest l q == DirUp   
                                                  then queryStructure ssa q 
                                                  else queryStructure ssb q
                             (LeafNode t)      -> t

