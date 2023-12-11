module Day6 where

import Data.Bifunctor (bimap, first)
import Data.Ix (inRange, range)
import Data.List (find, foldl')
import Data.List.Split (splitOn)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe (fromJust, mapMaybe)
import Data.Set (Set)
import qualified Data.Set as Set
import Debug.Trace (traceShow)

parseInput :: String -> [(Int, Int)]
parseInput s =
  let (times, distances) = bimap (map (read @Int) . drop 1 . words) (map (read @Int) . drop 1 . words) $ break (== '\n') s
   in zip times distances

-- simRace :: (Int, Int) -> 

day6 = do
  f <- readFile "input"
  print $ parseInput f
  -- part1 $ parseInput f
