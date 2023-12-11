{-# LANGUAGE OverloadedRecordDot #-}

module Day2 where

import Data.List (foldl')
import qualified Data.Map.Strict as Map
import Data.Void (Void)
import qualified Text.Megaparsec as M
import qualified Text.Megaparsec.Char as M

type Parser = M.Parsec Void String

data Game = Game
  { id :: Int
  , cubesSets :: [[(String, Int)]]
  }
  deriving (Show)

parseLine :: Parser Game
parseLine = do
  id <- M.string "Game" *> M.space *> parseNum <* M.char ':' <* M.space
  cubesSets <- M.sepBy parseSet (M.char ';') <* M.space
  pure Game{..}
 where
  parseSet = M.sepBy parseCube (M.char ',')
  parseCube = do
    count <- M.space *> parseNum
    color <- M.space *> M.some M.letterChar
    pure (color, count)
  parseNum :: Parser Int
  parseNum = read <$> M.some M.digitChar

type Cubes = Map.Map String Int

minRequirement :: Game -> Cubes
minRequirement game = foldl' (Map.unionWith max) Map.empty $ map Map.fromList game.cubesSets

power :: Cubes -> Int
power c = case (Map.lookup "red" c, Map.lookup "green" c, Map.lookup "blue" c) of
  (Just r, Just g, Just b) -> r * g * b
  _ -> error "WTF"

part2 = sum . map (power . minRequirement)

-- part1 :: [Game] -> Int
part1 = sum . map (\game -> game.id) . filter isPossible

isPossible :: Game -> Bool
isPossible game = all go game.cubesSets
 where
  go :: [(String, Int)] -> Bool
  go = all lessThanMax

lessThanMax :: (String, Int) -> Bool
lessThanMax (color, count) = case color of
  "green" -> count <= maxGreen
  "blue" -> count <= maxBlue
  "red" -> count <= maxRed
 where
  maxGreen = 13
  maxBlue = 14
  maxRed = 12

day2 = do
  f <- readFile "input"
  let Just input = M.parseMaybe (M.many parseLine) f
  pure $ part2 input
