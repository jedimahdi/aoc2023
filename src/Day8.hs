{-# LANGUAGE PartialTypeSignatures #-}
{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}

module Day8 where

import Data.Bifunctor (bimap, first)
import Data.List (foldl')
import Data.List.Split (splitOn)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Maybe (fromJust, mapMaybe)
import Data.Set (Set)
import Data.Set qualified as Set

type Node = String

type Nodes = Map Node (Node, Node)

data Instr = L | R
  deriving (Show)

step :: Nodes -> Instr -> Node -> Node
step nodes instr n = case Map.lookup n nodes of
  Nothing -> error "impossible"
  Just (leftNode, rightNode) -> case instr of
    L -> leftNode
    R -> rightNode

allNodes :: Nodes -> [Instr] -> [Node]
allNodes ns = loop "AAA" . cycle
  where
    loop currNode (i : is) =
      let
        newNode = step ns i currNode
       in
        currNode : loop newNode is
    loop _ [] = error "imposiblbe"

parseNodes :: String -> Nodes
parseNodes = Map.fromList . map parseNode . lines
  where
    parseNode :: String -> (Node, (Node, Node))
    parseNode s =
      let
        [lhs, rhs] = splitOn " = " s
        [l, r] = splitOn ", " $ tail $ init rhs
       in
        (lhs, (l, r))

parseInstrs :: String -> [Instr]
parseInstrs = map parseInstr
  where
    parseInstr :: Char -> Instr
    parseInstr 'R' = R
    parseInstr 'L' = L
    parseInstr _ = error "bad input"

parseInput :: String -> ([Instr], Nodes)
parseInput s =
  let
    [is, es] = splitOn "\n\n" s
   in
    (parseInstrs is, parseNodes es)

day8 :: IO ()
day8 = do
  rawInput <- readFile "input"
  let
    (instrs, ns) = parseInput rawInput

  print $ length $ takeWhile (/= "ZZZ") $ allNodes ns instrs
  -- print $ countNodesTill nodes instrs
  pure ()
