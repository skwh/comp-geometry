module
  Generator
  (
    randomNonIntersectingSegments
  )
where
  import Primitives
  import Predicates

  import System.Random

  randomPoint :: (Num a, Random a) => a -> StdGen -> Point a
  randomPoint max gen = let [x, y] = take 2 $ randomRs (1, max) gen
                        in (Point x y)

  randomSegment :: (Num a, Random a) => a -> StdGen -> LineSegment a
  randomSegment max gen = let [p1, p2] = [randomPoint max gen, randomPoint max $ fst . split $ gen]
                          in LineSegment p1 p2

  randomSegments :: (Num a, Random a) => Int -> a -> StdGen -> [LineSegment a]
  randomSegments 0 max gen = []
  randomSegments num max gen = (randomSegment max gen) : (randomSegments (num-1) max $ fst . split $ gen)

  checkIntersectsAny :: (Num a, Ord a, Random a) => LineSegment a -> [LineSegment a] -> Bool
  checkIntersectsAny l ls = foldl (||) False $ map (intersects l) ls

  makeNonIntersectingSegment :: (Num a, Ord a, Random a) => a -> StdGen -> [LineSegment a] -> LineSegment a
  makeNonIntersectingSegment max gen ls = let segment = randomSegment max gen
                                          in if checkIntersectsAny segment ls
                                             then makeNonIntersectingSegment max (fst . split $ gen) ls
                                             else segment

  randomNonIntersectingSegments :: (Num a, Ord a, Random a) => Int -> a -> StdGen -> [LineSegment a]
  randomNonIntersectingSegments num max gen = makeSegments' [] num max gen
    where makeSegments' prev 0 max gen = [makeNonIntersectingSegment max gen prev]
          makeSegments' prev num max gen = let seg = makeNonIntersectingSegment max gen prev
                                           in (seg : makeSegments' (seg:prev) (num-1) max (fst . split $ gen))
  

