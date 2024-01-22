{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}

module Day5 where

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

type Range = (Int, Int)

data Almanac = Almanac
  { seeds :: [Int]
  , maps :: [[(Range, Range)]]
  -- , seedToSoil :: [(Range, Range)]
  -- , soilToFertilizer :: [(Range, Range)]
  -- , fertilizerToWater :: [(Range, Range)]
  -- , water-to-light :: [(Range, Range)]
  -- , light-to-temperature :: [(Range, Range)]
  -- , temperature-to-humidity :: [(Range, Range)]
  -- , humidity-to-location :: [(Range, Range)]
  }
  deriving (Show)

parseInput :: String -> Almanac
parseInput s =
  let
    (seedsLine : mapsData) = splitOn "\n\n" s
    seeds = map (read @Int) $ drop 1 $ words seedsLine
    maps = map parseMap mapsData
   in
    Almanac {..}

parseMap :: String -> [(Range, Range)]
parseMap = map parseLine . drop 1 . lines
  where
    parseLine :: String -> (Range, Range)
    parseLine s =
      let
        [destRangeStart, srcRangeStart, rangeLength] = map (read @Int) $ words s
       in
        ((srcRangeStart, srcRangeStart + rangeLength - 1), (destRangeStart, destRangeStart + rangeLength - 1))

transform :: [Int] -> [(Range, Range)] -> [Int]
transform xs ranges = map go xs
  where
    go :: Int -> Int
    go x = case find (\(src, _) -> inRange src x) ranges of
      Nothing -> x
      Just ((srcStart, _srcEnd), (destStart, _destEnd)) -> destStart + (x - srcStart)

toPairs :: [Int] -> [(Int, Int)]
toPairs (x : y : xs) = (x, y) : toPairs xs
toPairs [_] = []
toPairs [] = []

day5 :: IO ()
day5 = do
  input <- parseInput <$> readFile "input"
  print input.seeds
  print $ minimum $ foldl' transform input.seeds input.maps
  print $ minimum $ foldl' transform (concatMap (\(start, len) -> range (start, start + len - 1)) $ toPairs input.seeds) input.maps
