module Main 
  (
    main
  , readSegmentFile
  ) 
  where

  import System.IO (readFile)
  import System.Environment (getArgs)
  import System.Console.Readline ( readline, addHistory )
  import Control.Monad (when)
  import Data.IntMap.Lazy (IntMap)
  import qualified Data.IntMap.Lazy as IntMap

  import PointLocation
  import Primitives
  import Predicates
  import Generator

  main :: IO ()
  main = do
    [fileName] <- getArgs
    segs <- readSegmentFile fileName
    putStrLn $ "Read " ++ (show $ length segs) ++ " segments."
    let (min, max) = findMinMax segs
        newPair = emptyRefinementPair min max
        finalPair = foldl (rebuildRefinementPair) newPair segs
    putStrLn $ "Trapezoidal Map contains " ++ (show $ IntMap.size (fst finalPair)) ++ " trapezoids." 
    putStrLn $ "Min point is " ++ (show $ min) ++ " and max point is " ++ (show $ max) ++ "."
    putStrLn "Read File Successfully."
    repl finalPair

  repl :: (Num a, Ord a, Show a, Read a) => RefinementPair a -> IO ()
  repl pair = do
    putStrLn "Please input the coordinates of a point to query the data structure."
    Just line <- readline "> "
    when (not $ null line) $ do
      addHistory line
      let [a,b] = map (read) $ words line
          point = Point a b
      putStrLn $ show $ doQuery pair point
      repl pair

  minPoint :: (Ord a) => Point a -> Point a -> Point a -> Point a
  minPoint p1 p2 p3 = min (min p1 p2) p3

  maxPoint :: (Ord a) => Point a -> Point a -> Point a -> Point a
  maxPoint p1 p2 p3 = max (max p1 p2) p3

  findMinMax :: (Ord a) => [LineSegment a] -> (Point a, Point a)
  findMinMax ((LineSegment p1 p2):ls) = findMinMax' ((LineSegment p1 p2):ls) (p1, p2)
      where findMinMax' [] pair = pair
            findMinMax' ((LineSegment p1 p2):ls) (pmin, pmax) = (minPoint p1 p2 pmin, maxPoint p1 p2 pmax)

  readSegment :: (Read a) => String -> LineSegment a
  readSegment str = let [p1, p2, p3, p4] = map (read) $ words str
                    in LineSegment (Point p1 p2) (Point p3 p4)

  readSegments :: (Read a) => [String] -> [LineSegment a]
  readSegments [] = []
  readSegments (x:xs) = (readSegment x) : readSegments xs

  readSegmentFile :: (Read a) => String -> IO [LineSegment a]
  readSegmentFile fileName = readSegments <$> tail.lines <$> readFile fileName
