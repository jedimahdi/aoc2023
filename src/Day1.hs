module Day1 where

import Data.Char (digitToInt, isDigit)
import Data.Maybe
import Data.Monoid

main = do
  f <- readFile "input"
  let sol1 = part2 f
  let x = sum [1, 2]
  print sol1
  -- pure $ zip (part2 f) (lines f)

part1 :: String -> Int
part1 s =
  let ls = lines s
   in sum $ map (\l -> read [fromJust $ firstNum l, fromJust $ lastNum l]) ls

part2 :: String -> Int
part2 = sum . map ((\ns -> read (show (head ns) ++ show (last ns))) . parseNums) . lines

parseNums :: String -> [Int]
parseNums [] = []
parseNums (c : cs) | isDigit c = digitToInt c : parseNums cs
parseNums ('o' : 'n' : 'e' : cs) = 1 : parseNums ('n' : 'e' : cs)
parseNums ('t' : 'w' : 'o' : cs) = 2 : parseNums ('w' : 'o' : cs)
parseNums ('t' : 'h' : 'r' : 'e' : 'e' : cs) = 3 : parseNums ('h' : 'r' : 'e' : 'e' : cs)
parseNums ('f' : 'o' : 'u' : 'r' : cs) = 4 : parseNums ('o' : 'u' : 'r' : cs)
parseNums ('f' : 'i' : 'v' : 'e' : cs) = 5 : parseNums ('i' : 'v' : 'e' : cs)
parseNums ('s' : 'i' : 'x' : cs) = 6 : parseNums ('i' : 'x' : cs)
parseNums ('s' : 'e' : 'v' : 'e' : 'n' : cs) = 7 : parseNums ('e' : 'v' : 'e' : 'n' : cs)
parseNums ('e' : 'i' : 'g' : 'h' : 't' : cs) = 8 : parseNums ('i' : 'g' : 'h' : 't' : cs)
parseNums ('n' : 'i' : 'n' : 'e' : cs) = 9 : parseNums ('i' : 'n' : 'e' : cs)
parseNums (c : cs) = parseNums cs

foldNums :: (Monoid m) => (Char -> m) -> String -> m
foldNums m = foldMap (\c -> if isDigit c then m c else mempty)

firstNum :: String -> Maybe Char
firstNum = getFirst . foldNums (First . Just)

lastNum :: String -> Maybe Char
lastNum = getLast . foldNums (Last . Just)
