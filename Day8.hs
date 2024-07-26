module Day8 where

import Data.Map qualified as M
import Data.Maybe (fromMaybe)
import GHC.Base (Alternative ((<|>)))
import Parser

data Instruction = L | R deriving (Show)

parseInstruction = (L <$ parseChar 'L') <|> (R <$ parseChar 'R')

startNode = "AAA"

finalNode = "ZZZ"

parseNode = parseN 3 parseId

parseRule = do
  skipWS
  from <- parseNode
  skipWS
  parseChar '='
  skipWS
  parseChar '('
  left <- parseNode
  skipWS
  parseChar ','
  skipWS
  right <- parseNode
  skipWS
  parseChar ')'
  return (from, (left, right))

parseRules = do
  ins <- parseStar parseInstruction
  rules <- parseStar parseRule
  return (ins, rules)

--

q input = snd val
  where
    res = runParser parseRules input
    val = fromMaybe undefined res

testInput = "LR\n11A = (11B, XXX)\n11B = (XXX, 11Z)\n11Z = (11B, XXX)\n22A = (22B, XXX)\n22B = (22C, 22C)\n22C = (22Z, 22Z)\n22Z = (22B, 22B)\nXXX = (XXX, XXX)"

walk documents startNode ins = aux 0 ins startNode
  where
    aux cnt is cur
      | last cur == 'Z' = cnt
      | otherwise = case head is of
          L -> next left
          R -> next right
      where
        next = aux (cnt + 1) (tail is)
        (left, right) = documents M.! cur

findStartNodes rules = aux rules []
  where
    aux [] startNodes = startNodes
    aux (r : rs) startNodes
      | last r == 'A' = aux rs (r : startNodes)
      | otherwise = aux rs startNodes

solver input = walk documents startNode $ cycle inst
  where
    (inst, rules) = q input
    documents = M.fromList rules

solver2 input = foldl1 lcm $ map (\start -> walk documents start $ cycle inst) startNodes
  where
    (inst, rules) = q input
    documents = M.fromList rules
    startNodes = findStartNodes $ map fst rules

testFilename = "day8-test.txt"

filename = "day8.txt"

day8 solver filename = do
  contents <- readFile filename
  print $ solver contents
