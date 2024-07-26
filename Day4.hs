module Day4 where

import Data.Char
import Data.Set qualified as S
import GHC.Base (Alternative (..))
import Parser

data Card = Card (S.Set Integer) (S.Set Integer) deriving (Show)

cardWorth (Card my winning) = (\i -> if i > 0 then 2 ^ (i - 1) else 0) $ S.size $ S.intersection winning my

cardWorth2 (Card my winning) = S.size $ S.intersection winning my

parseCard = do
  skipWS
  parseString "Card"
  skipWS
  parseNumber
  parseChar ':'
  winningNumbers <- parseStar numbersStar
  skipWS
  parseChar '|'
  skipWS
  myNumbers <- parseStar numbersStar
  return (Card (S.fromList myNumbers) (S.fromList winningNumbers))
  where
    numbersStar = do
      skipWS
      parseNumber

testInput = "Card 1: 41 48 83 86 17 | 83 86  6 31 17  9 48 53\nCard 2: 13 32 20 16 61 | 61 30 68 82 17 32 24 19\nCard 3:  1 21 53 59 44 | 69 82 63 72 16 21 14  1\nCard 4: 41 92 73 84 69 | 59 84 76 51 58  5 54 83\nCard 5: 87 83 26 28 32 | 88 30 70 12 93 22 82 36\nCard 6: 31 18 13 56 72 | 74 77 10 23 35 67 36 11"

parseCards = parseStar parseCard

solve input = case runParser parseCards input of
  Just (_, cards) -> sum $ map cardWorth cards
  _ -> -1

solve2 input = case runParser parseCards input of
  Just (_, cards) -> sum $ countCards (replicate (length cards) 1) $ map cardWorth2 cards
  _ -> -1
  where
    countCards counts [] = counts
    countCards (count : counts) (worth : t) = count : countCards (addCounts counts worth count) t
    addCounts [] _ _ = []
    addCounts c@(count : counts) worth add
      | worth == 0 = c
      | otherwise = (count + add) : addCounts counts (worth - 1) add

day4 :: (Show a) => (String -> a) -> FilePath -> IO ()
day4 solver filename = do
  contents <- readFile filename
  print $ solver contents
