module Day7 where

import Control.Applicative (asum, (<|>))
import Data.Char (chr, ord)
import Data.List (sort)
import Data.Map qualified as M
import Data.Maybe (fromMaybe)
import Data.Set qualified as S
import Parser

newtype Card = Card Integer deriving (Eq, Ord)

int2Char i = chr (ord '0' + i)

cardSymbols = map int2Char [2 .. 9] ++ "TJQKA"

cardSymbols2 = "J" ++ map int2Char [2 .. 9] ++ "TQKA"

instance Show Card where
  show (Card i) = [cardToSymbol M.! i]

cardToSymbol = M.fromList $ zip [2 ..] cardSymbols

newtype Hand = Hand [Card] deriving (Eq, Ord)

instance Show Hand where
  show (Hand cards) = concatMap show cards

newtype TypedHand = TypedHand (Integer, Hand) deriving (Eq, Ord, Show)

data Play = Play TypedHand Integer deriving (Eq, Ord, Show)

symbolToCard = M.fromList $ zip cardSymbols [2 ..]

parseCard = do
  Card . (M.!) symbolToCard <$> parseId

parseHand = Hand <$> parseN 5 parseCard

parsePlay = do
  skipWS
  hand <- parseHand
  skipWS
  bid <- parseNumber
  return (hand, bid)

parseHands =
  parseStar
    ( do
        skipWS
        hand <- parseHand
        skipWS
        parseNumber
        return hand
    )

parsePlays = parseStar parsePlay

testInput = "32T3K 765\nT55J5 684\nKK677 28\nKTJJT 220\nQQQJA 483"

queen = Card $ symbolToCard M.! 'Q'

king = Card $ symbolToCard M.! 'K'

testFive = Hand $ replicate 5 queen

testFour = Hand $ king : replicate 4 queen

frequency xs = M.fromListWith (+) [(x, 1) | x <- xs]

maybeSubSet weight counts (Hand cards)
  | length counts == length fr && sort (M.elems fr) == counts = Just weight
  | otherwise = Nothing
  where
    fr = frequency cards

maybeFiveKind = maybeSubSet 6 [5]

maybeFourKind = maybeSubSet 5 [1, 4]

maybeFullHouse = maybeSubSet 4 [2, 3]

maybeThreeKind = maybeSubSet 3 [1, 1, 3]

maybeTwoPairs = maybeSubSet 2 [1, 2, 2]

maybeOnePair = maybeSubSet 1 [1, 1, 1, 2]

getHandType hand = fromMaybe 0 $ asum $ map (\f -> f hand) types
  where
    types = [maybeFiveKind, maybeFourKind, maybeFullHouse, maybeThreeKind, maybeTwoPairs, maybeOnePair]

typePlays plays = [Play (TypedHand (getHandType hand, hand)) i | (hand, i) <- plays]

solve input = sum $ [i * bid | (Play _ bid, i) <- zip (sort typedPlays) [1 .. toInteger $ length typedPlays]]
  where
    typedPlays = typePlays $ q input

---

q input = snd val
  where
    res = runParser parsePlays input
    val = fromMaybe undefined res

day7 filename = do
  contents <- readFile filename
  print $ solve contents
