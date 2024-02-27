{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use any" #-}

module Day3 where

import Data.Array (Array)
import Data.Array qualified as A
import Data.Bifunctor (bimap)
import Data.Char (isDigit)
import Data.Ix qualified as I
import Data.List (foldl')
import Data.List qualified as L
import Data.Map.Strict qualified as Map
import Data.Maybe (mapMaybe)
import Data.Set qualified as Set
import Data.Void (Void)
import Text.Megaparsec qualified as M
import Text.Megaparsec.Char qualified as M

type Pos = (Int, Int)

type Input = ([(Int, [Pos])], Set.Set Pos)

type Parser = M.ParsecT Void String IO

mkArray :: String -> Array Pos Char
mkArray s = A.listArray bounds $ concat $ lines s
  where
    bounds = ((0, 0), (length (head (lines s)) - 1, length (lines s) - 1))

groupDigits :: [(Pos, Char)] -> [[(Pos, Char)]]
groupDigits =
  go False
  where
    go _ [] = []
    go False xs =
      let
        (notNumbers, withNumbers) = break (isDigit . snd) xs
       in
        notNumbers : go True withNumbers
    go True xs =
      let
        (numbers, rest) = span (isDigit . snd) xs
       in
        numbers : go False rest

readNumber :: [(b, Char)] -> (Int, [b])
readNumber xs =
  let
    poses = map fst xs
    n = read @Int (map snd xs)
   in
    (n, poses)

neighbors :: Pos -> [Pos]
neighbors (y, x) = [(a, b) | a <- [y + 1, y, y - 1], b <- [x + 1, x, x - 1]]

isPartNumber :: Set.Set Pos -> (Int, [Pos]) -> Bool
isPartNumber symbols (snd -> number) = any (`Set.member` symbols) $ concatMap neighbors number

findGearRatio :: Map.Map Pos Int -> Pos -> [Int]
findGearRatio numbers p = L.nub $ mapMaybe (`Map.lookup` numbers) (neighbors p)

day3 :: IO ()
day3 = do
  f <- readFile "input"
  let
    (numbers, symbols) =
      bimap
        (map readNumber)
        (filter ((/= '.') . snd) . concat)
        $ L.partition (isDigit . snd . head)
        $ filter (not . null)
        $ groupDigits
        $ A.assocs
        $ mkArray f
  let
    symbolsPosSet = Set.fromList . map fst $ symbols
  let
    numbersMap = foldl' Map.union Map.empty (map (\(n, ps) -> foldl' (\m p -> Map.insert p n m) Map.empty ps) numbers)
  let
    gearSymbols = map fst $ filter ((== '*') . snd) symbols
  print $ sum $ map fst $ filter (isPartNumber symbolsPosSet) numbers

  print $ sum $ map product $ filter ((== 2) . length) $ map (findGearRatio numbersMap) gearSymbols
