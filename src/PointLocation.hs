module
  PointLocation
  (
    emptyRefinementPair
  , rebuildRefinementPair
  , doQuery
  , RefinementPair (..)
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

  scanAndReassign :: Int -> Int -> TrapezoidMap a -> TrapezoidMap a
  scanAndReassign old new tm = IntMap.delete old . reassign old new $ tm
    where reassign :: Int -> Int -> TrapezoidMap a -> TrapezoidMap a
          reassign old new tm = IntMap.map (\x -> updateUpperLeftNeighborField x old new) .
                                IntMap.map (\x -> updateLowerLeftNeighborField x old new) .
                                IntMap.map (\x -> updateUpperRightNeighborField x old new) .
                                IntMap.map (\x -> updateLowerRightNeighborField x old new) $ tm

  updateUpperLeftNeighborField :: Trapezoid a -> IntMap.Key -> IntMap.Key -> Trapezoid a
  updateUpperLeftNeighborField t ok nk = if upperLeftNeighbor t == Just ok
                                           then t { upperLeftNeighbor = Just nk }
                                           else t 

  updateLowerLeftNeighborField :: Trapezoid a -> IntMap.Key -> IntMap.Key -> Trapezoid a
  updateLowerLeftNeighborField t ok nk = if lowerLeftNeighbor t == Just ok
                                           then t { lowerLeftNeighbor = Just nk }
                                           else t 

  updateUpperRightNeighborField :: Trapezoid a -> IntMap.Key -> IntMap.Key -> Trapezoid a
  updateUpperRightNeighborField t ok nk = if upperRightNeighbor t == Just ok
                                           then t { upperRightNeighbor = Just nk }
                                           else t 

  updateLowerRightNeighborField :: Trapezoid a -> IntMap.Key -> IntMap.Key -> Trapezoid a
  updateLowerRightNeighborField t ok nk = if lowerRightNeighbor t == Just ok
                                           then t { lowerRightNeighbor = Just nk }
                                           else t 

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

  -- insert a segment into the map and search structure
  rebuildRefinementPair :: (Ord a, Num a) => RefinementPair a -> LineSegment a -> RefinementPair a
  rebuildRefinementPair rp ls = processTrapezoids rp (followSegment rp ls) ls

  -- Determine which trapezoids lie along a segment
  followSegment :: (Ord a, Num a) => RefinementPair a -> LineSegment a -> [Trapezoid a]
  followSegment (tm, ss) (LineSegment p q) = followSegment' [tm IntMap.! queryStructure ss p] q
    where followSegment' []     q = undefined -- this should never happen
          followSegment' (t:ts) q = case xNodeTest (rightP t) q
                                      of DirRight -> if yNodeTest (bottom t) q == DirUp
                                                     then t : followSegment' ((neighboringTrapezoidFromMap DirDown tm t) : ts) q
                                                     else t : followSegment' ((neighboringTrapezoidFromMap DirUp tm t) : ts) q
                                         DirLeft  -> (t:ts)
          neighboringTrapezoidFromMap :: VerticalDirection -> TrapezoidMap a -> Trapezoid a -> Trapezoid a
          neighboringTrapezoidFromMap DirUp   tm t = tm IntMap.! fromJust (upperRightNeighbor t) -- Errors are being raised here bcause these trapezoids do not exist in the map
          neighboringTrapezoidFromMap DirDown tm t = tm IntMap.! fromJust (lowerRightNeighbor t) -- Or, because a trapezoid does not have a neighbor when it should. This is a reassignment issue

  processTrapezoids :: (Ord a, Num a) => RefinementPair a -> [Trapezoid a] -> LineSegment a -> RefinementPair a
  -- Case: trapezoid entirely contains the segment
  processTrapezoids p [t] ls = bcts p t ls
  -- Case: left endpoint
  processTrapezoids (tm, ss) (t:ts) (LineSegment p q) = let tm' = scanAndReassign (trapezoidId t) leftId . IntMap.insert leftId newTrapezoidLeft .
                                                                                                           IntMap.insert topId newTrapezoidTop   .
                                                                                                           IntMap.insert bottomId newTrapezoidBottom $ tm
                                                            ss' = (XNode p (LeafNode $ trapezoidId newTrapezoidLeft) 
                                                                           (YNode (LineSegment p q) 
                                                                                  (LeafNode $ trapezoidId newTrapezoidTop) (LeafNode $ trapezoidId newTrapezoidBottom)))
                                                        in processLeftToRight (tm', appendAtTrapezoid ss ss' p) (LineSegment p q) (newTrapezoidTop, newTrapezoidBottom) ts
    where [leftId, topId, bottomId] = getNewIds tm 3
          newTrapezoidLeft = Trapezoid { trapezoidId = leftId
                                       , top = top t
                                       , bottom = bottom t
                                       , leftP = leftP t
                                       , rightP = p
                                       , upperLeftNeighbor = upperLeftNeighbor t
                                       , lowerLeftNeighbor = lowerLeftNeighbor t
                                       , upperRightNeighbor = Just topId
                                       , lowerRightNeighbor = Just bottomId
                                       }
          newTrapezoidTop = Trapezoid { trapezoidId = topId
                                      , top = top t
                                      , bottom = LineSegment p q
                                      , leftP = p
                                      , rightP = rightP t
                                      , upperLeftNeighbor = Just leftId
                                      , lowerLeftNeighbor = Nothing
                                      , upperRightNeighbor = upperRightNeighbor t
                                      , lowerRightNeighbor = Nothing 
                                      }
          newTrapezoidBottom = Trapezoid { trapezoidId = bottomId
                                         , top = LineSegment p q
                                         , bottom = bottom t
                                         , leftP = p
                                         , rightP = rightP t
                                         , upperLeftNeighbor = Nothing
                                         , lowerLeftNeighbor = Just leftId
                                         , upperRightNeighbor = Nothing
                                         , lowerRightNeighbor = lowerRightNeighbor t
                                         }

  -- Split a trapezoid along a given line segment
  splitTrapezoid :: Trapezoid a -> TrapezoidMap a -> LineSegment a -> (Trapezoid a, Trapezoid a, TrapezoidMap a)
  splitTrapezoid t tm (LineSegment p q) = let tm' = scanAndReassign (trapezoidId t) topId . IntMap.insert topId newTrapezoidTop .
                                                                                            IntMap.insert bottomId newTrapezoidBottom $ tm
                                          in (newTrapezoidTop, newTrapezoidBottom, tm')
    where [topId, bottomId] = getNewIds tm 2
          newTrapezoidTop = Trapezoid { trapezoidId = topId
                                      , top = top t
                                      , bottom = LineSegment p q
                                      , leftP = leftP t
                                      , rightP = rightP t
                                      , upperLeftNeighbor = upperLeftNeighbor t
                                      , lowerLeftNeighbor = Nothing
                                      , upperRightNeighbor = upperRightNeighbor t
                                      , lowerRightNeighbor = Nothing
                                      }
          newTrapezoidBottom = Trapezoid { trapezoidId = bottomId
                                         , top = LineSegment p q
                                         , bottom = bottom t
                                         , leftP = leftP t
                                         , rightP = rightP t
                                         , upperLeftNeighbor = Nothing
                                         , lowerLeftNeighbor = lowerLeftNeighbor t
                                         , upperRightNeighbor = Nothing
                                         , lowerRightNeighbor = lowerRightNeighbor t
                                         }
  
  processLeftToRight :: (Eq a, Ord a, Num a) => RefinementPair a -> LineSegment a -> (Trapezoid a, Trapezoid a) -> [Trapezoid a] -> RefinementPair a
  -- Case: Right endpoint
  processLeftToRight (tm, ss) (LineSegment p q) (topT, bottomT) [t] = let tm' = scanAndReassign (trapezoidId t) rightId . IntMap.insert rightId newTrapezoidRight   .
                                                                                                                          IntMap.insert topId newTrapezoidTop       .
                                                                                                                          IntMap.insert bottomId newTrapezoidBottom $ tm
                                                                          ss' = (XNode q (YNode (LineSegment p q) 
                                                                                                (LeafNode (trapezoidId newTrapezoidTop)) (LeafNode (trapezoidId newTrapezoidBottom))) 
                                                                                          (LeafNode (trapezoidId newTrapezoidRight)))
                                                                      in (tm', appendAtTrapezoid ss ss' q) 
    where [topId, bottomId, rightId] = getNewIds tm 3
          newTrapezoidTop = Trapezoid { trapezoidId = topId
                                      , top = top t
                                      , bottom = (LineSegment p q)
                                      , leftP = leftP t
                                      , rightP = rightP t
                                      , upperLeftNeighbor = upperLeftNeighbor t
                                      , lowerLeftNeighbor = Nothing
                                      , upperRightNeighbor = upperRightNeighbor t
                                      , lowerRightNeighbor = Nothing
                                      }
          newTrapezoidBottom = Trapezoid { trapezoidId = bottomId 
                                         , top = (LineSegment p q)
                                         , bottom = bottom t
                                         , leftP = leftP t
                                         , rightP = rightP t
                                         , upperLeftNeighbor = Nothing
                                         , lowerLeftNeighbor = lowerLeftNeighbor t
                                         , upperRightNeighbor = Nothing
                                         , lowerRightNeighbor = lowerRightNeighbor t
                                         }
          newTrapezoidRight = Trapezoid { trapezoidId = rightId
                                        , top = top t
                                        , bottom = bottom t
                                        , leftP = q
                                        , rightP = rightP t
                                        , upperLeftNeighbor = Just topId
                                        , lowerLeftNeighbor = Just bottomId
                                        , upperRightNeighbor = upperRightNeighbor t
                                        , lowerRightNeighbor = lowerRightNeighbor t
                                        }
  -- Case: middle trapezoids
  processLeftToRight (tm, ss) (LineSegment p q) (topT, bottomT) (t:ts) = let (nt, nb, tm') = splitTrapezoid t tm (LineSegment p q)
                                                                             tm'' = mergeTop topT nt tm'
                                                                             tm''' = mergeBottom bottomT nb tm''
                                                                             ss' = (YNode (LineSegment p q) (LeafNode (trapezoidId nt)) (LeafNode (trapezoidId nb)))
                                                                         in processLeftToRight (tm''', appendAtTrapezoid ss ss' (leftP nt)) (LineSegment p q) (nt, nb) ts 
    where mergeTop pTop nTop tm = if top pTop == top nTop
                                  then IntMap.delete (trapezoidId nTop) . IntMap.adjust (\x -> modifiedTopTrapezoid) (trapezoidId pTop) $ tm
                                  else tm
            where modifiedTopTrapezoid = Trapezoid { trapezoidId = trapezoidId pTop
                                                   , top = top pTop
                                                   , bottom = (LineSegment p q)
                                                   , leftP = leftP pTop
                                                   , rightP = rightP nTop
                                                   , upperLeftNeighbor = upperLeftNeighbor pTop
                                                   , lowerLeftNeighbor = lowerLeftNeighbor pTop
                                                   , upperRightNeighbor = upperRightNeighbor nTop
                                                   , lowerRightNeighbor = lowerRightNeighbor nTop
                                                   }
          mergeBottom pBot nBot tm = if bottom pBot == bottom nBot
                                     then IntMap.delete (trapezoidId nBot) . IntMap.adjust (\x -> modifiedBottomTrapezoid) (trapezoidId pBot) $ tm
                                     else tm
            where modifiedBottomTrapezoid = Trapezoid { trapezoidId = trapezoidId pBot
                                                      , top = (LineSegment p q)
                                                      , bottom = bottom pBot
                                                      , leftP = leftP pBot
                                                      , rightP = rightP nBot
                                                      , upperLeftNeighbor = upperLeftNeighbor pBot
                                                      , lowerLeftNeighbor = lowerLeftNeighbor pBot
                                                      , upperRightNeighbor = upperRightNeighbor nBot
                                                      , lowerRightNeighbor = lowerRightNeighbor nBot
                                                      }

  -- Break Containing Trapezoid Segment: case when a trapezoid completely contains a segment
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
    where [aId, bId, cId, dId] = getNewIds tm 4
          newTrapezoidA = Trapezoid { trapezoidId = aId
                                    , top = top t
                                    , bottom = bottom t
                                    , leftP = leftP t
                                    , rightP = p
                                    , upperLeftNeighbor = upperLeftNeighbor t
                                    , lowerLeftNeighbor = lowerLeftNeighbor t
                                    , upperRightNeighbor = Just cId
                                    , lowerRightNeighbor = Just dId
                                    }
          newTrapezoidB = Trapezoid { trapezoidId = bId
                                    , top = top t
                                    , bottom = bottom t
                                    , leftP = q
                                    , rightP = rightP t
                                    , upperLeftNeighbor = Just cId
                                    , lowerLeftNeighbor = Just dId
                                    , upperRightNeighbor = upperRightNeighbor t
                                    , lowerRightNeighbor = lowerRightNeighbor t
                                    }
          newTrapezoidC = Trapezoid { trapezoidId = cId
                                    , top = top t
                                    , bottom = LineSegment p q
                                    , leftP = p
                                    , rightP = q
                                    , upperLeftNeighbor = Just aId
                                    , lowerLeftNeighbor = Nothing
                                    , upperRightNeighbor = Just bId
                                    , lowerRightNeighbor = Nothing
                                    }
          newTrapezoidD = Trapezoid { trapezoidId = dId
                                    , top = LineSegment p q
                                    , bottom = bottom t
                                    , leftP = p
                                    , rightP = q
                                    , upperLeftNeighbor = Nothing
                                    , lowerLeftNeighbor = Just aId
                                    , upperRightNeighbor = Nothing
                                    , lowerRightNeighbor = Just bId
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

  doQuery :: (Ord a, Num a) => RefinementPair a -> Point a -> Maybe (Trapezoid a)
  doQuery (tm, ss) p = (queryStructure ss p) `IntMap.lookup` tm
