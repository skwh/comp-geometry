module Main 
  (
    main
  ) 
  where

  import System.IO (readFile)
  import System.Environment (getArgs)

  import PointLocation
  import Primitives
  import Predicates
  import Generator

  main :: IO ()
  main = do
    [fileName] <- getArgs
    segs <- readSegmentFile fileName
    let (min, max) = findMinMax segs
        newPair = emptyRefinementPair min max
        result = foldl (rebuildRefinementPair) newPair segs
    putStrLn "Success"

  findMinMax :: (Ord a) => [LineSegment a] -> (Point a, Point a)
  findMinMax segs = undefined

  readSegment :: (Read a) => String -> LineSegment a
  readSegment str = let [p1, p2, p3, p4] = map (read) $ words str
                    in LineSegment (Point p1 p2) (Point p3 p4)

  readSegments :: (Read a) => [String] -> [LineSegment a]
  readSegments [] = []
  readSegments (x:xs) = (readSegment x) : readSegments xs

  readSegmentFile :: (Read a) => String -> IO [LineSegment a]
  readSegmentFile fileName = readSegments <$> lines <$> readFile fileName
