module Day4 where

import Data.Bifunctor (bimap, first)
import Data.List (foldl')
import Data.Map.Strict qualified as Map
import Data.Maybe (fromJust, mapMaybe)
import Data.Set (Set)
import Data.Set qualified as Set
import Debug.Trace (traceShow)
import Prelude hiding (id)

type WinningNumbers = Set Int

type Numbers = [Int]

data Card = Card
  { id :: Int
  , winningNumbers :: WinningNumbers
  , numbers :: Numbers
  }
  deriving (Show)

doubleScore :: Int -> Int
doubleScore 0 = 1
doubleScore n = n * 2

score :: Int -> Int
score 0 = 0
score n = iterate doubleScore 1 !! (n - 1)

cardWins :: Card -> Int
cardWins card = foldl' go 0 card.numbers
  where
    go :: Int -> Int -> Int
    go count n
      | n `Set.member` card.winningNumbers = count + 1
      | otherwise = count

parseLine :: String -> Card
parseLine s =
  let
    (cardIdInfo, nums) = break (== ':') s
    id = read @Int . head . words . drop 4 $ cardIdInfo
    (winningNumbers, numbers) = bimap (Set.fromList . map (read @Int) . words) (map (read @Int) . words . drop 1) . break (== '|') . drop 1 $ nums
   in
    Card {..}

day4 :: IO Int
day4 = do
  f <- readFile "input"
  let
    cards = map parseLine $ lines f
  print $ sum $ map (score . cardWins) cards
  let
    x = foldl' go (Map.fromList (map (\c -> (c.id, 1)) cards)) cards
  pure $ sum $ Map.elems x
  where
    go :: Map.Map Int Int -> Card -> Map.Map Int Int
    go m card = inc card.id wins (+ copies) m
      where
        wins = cardWins card
        copies = fromJust $ Map.lookup card.id m

inc :: Int -> Int -> (Int -> Int) -> Map.Map Int Int -> Map.Map Int Int
inc _ 0 _ m = m
inc from n f m = Map.adjust f (from + n) (inc from (n - 1) f m)
