module Day9 where

import Control.Applicative (Alternative (many, some))
import Parser

testInput = "0 3 6 9 12 15\n1 3 6 10 15 21\n10 13 16 21 30 45"

parseSeq = parseStarDelim ' ' parseNumber

parseSeqs = parseStarDelim '\n' parseSeq

extrapolate seq
  | all (== head seq) seq = head seq
  | otherwise = last seq + extrapolate (differences seq)

differences seq = zipWith (-) (tail seq) seq

solver extrapolateMethod input = case runParser parseSeqs input of
  Just (_, seqs) -> sum $ map extrapolateMethod seqs
  _ -> undefined

solver1 = solver extrapolate

solver2 = solver (extrapolate . reverse)

filename = "day9.txt"

day9 solver filename = do
  contents <- readFile filename
  print $ solver contents
