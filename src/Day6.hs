module Day6 where

import Data.Bifunctor (bimap, first)
import Data.Ix (inRange, range)
import Data.List (find, foldl')
import Data.List.Split (splitOn)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Maybe (fromJust, mapMaybe)
import Data.Set (Set)
import Data.Set qualified as Set
import Debug.Trace (traceShow)

parseInput :: String -> [(Int, Int)]
parseInput s =
  let
    (times, distances) = bimap (map (read @Int) . drop 1 . words) (map (read @Int) . drop 1 . words) $ break (== '\n') s
   in
    zip times distances

-- simRace :: (Int, Int) ->

day6 :: IO ()
day6 = do
  f <- readFile "input"
  print $ parseInput f

-- part1 $ parseInput f
