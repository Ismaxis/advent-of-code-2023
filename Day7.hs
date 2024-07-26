module Day7 where

import Control.Applicative (asum, (<|>))
import Data.Char (chr, ord)
import Data.Functor (($>))
import Data.List (sort)
import Data.Map qualified as M
import Data.Maybe (fromMaybe)
import Data.Set qualified as S
import Parser

newtype Card = Card Integer deriving (Eq, Ord)

int2Char i = chr (ord '0' + i)

cardSymbols = map int2Char [2 .. 9] ++ "TJQKA"

instance Show Card where
  show (Card i) = " " ++ show i

cardToSymbol = M.fromList $ zip [2 ..] cardSymbols

cardToSymbol2 = M.fromList $ zip [2 ..] cardSymbols2

jokerSymbol = 'J'

cardSymbols2 = [jokerSymbol] ++ map int2Char [2 .. 9] ++ "TQKA"

jokerNumber = symbolToCard2 M.! jokerSymbol

jokerCard = Card jokerNumber

newtype Hand = Hand [Card] deriving (Eq, Ord)

instance Show Hand where
  show (Hand cards) = concatMap show cards

newtype TypedHand = TypedHand (Integer, Hand) deriving (Eq, Ord, Show)

data Play = Play TypedHand Integer deriving (Eq, Ord, Show)

symbolToCard = M.fromList $ zip cardSymbols [2 ..]

symbolToCard2 = M.fromList $ zip cardSymbols2 [12 ..]

parseCard symbolToCard = do
  skipWS
  Card . (M.!) symbolToCard <$> parseId

parseHand symbolToCard = Hand <$> parseN 5 (parseCard symbolToCard)

parsePlay symbolToCard = do
  skipWS
  hand <- parseHand symbolToCard
  skipWS
  bid <- parseNumber
  return (hand, bid)

parsePlays symbolToCard = parseStar $ parsePlay symbolToCard

frequency xs = M.fromListWith (+) [(x, 1) | x <- xs]

subSetSizes (Hand cards) = sort $ M.elems fr
  where
    fr = frequency cards

subSetSizes2 hand@(Hand cards) = case M.lookup jokerCard fr of
  Just jokersCount -> if jokersCount == 5 then [5] else updateLast (jokersCount +) $ sort $ M.elems frWithoutJokers
    where
      frWithoutJokers = M.delete jokerCard fr
  _ -> subSetSizes hand
  where
    fr = frequency cards
    updateLast _ [] = []
    updateLast f [l] = [f l]
    updateLast f (h : t) = h : updateLast f t

maybeSubSet weight counts sizes
  | length counts == length sizes && sizes == counts = Just weight
  | otherwise = Nothing

maybeFiveKind = maybeSubSet 6 [5]

maybeFourKind = maybeSubSet 5 [1, 4]

maybeFullHouse = maybeSubSet 4 [2, 3]

maybeThreeKind = maybeSubSet 3 [1, 1, 3]

maybeTwoPairs = maybeSubSet 2 [1, 2, 2]

maybeOnePair = maybeSubSet 1 [1, 1, 1, 2]

getHandType subSetFunc hand = fromMaybe 0 $ asum $ map (\f -> f $ subSetFunc hand) types
  where
    types = [maybeFiveKind, maybeFourKind, maybeFullHouse, maybeThreeKind, maybeTwoPairs, maybeOnePair]

handType = getHandType subSetSizes

handType2 = getHandType subSetSizes2

typePlays handType plays = [Play (TypedHand (handType hand, hand)) i | (hand, i) <- plays]

typePlays2 = typePlays handType2

solve handType symbolToCard input = sum [i * bid | (Play _ bid, i) <- zip (sort typedPlays) coefs]
  where
    typedPlays = typePlays handType $ q symbolToCard input
    len = length typedPlays
    -- coefs = replicate len 1
    coefs = [1 .. toInteger len]

solver1 = solve handType symbolToCard

solver2 = solve handType2 symbolToCard2

---

q symbolToCard input = snd val
  where
    res = runParser (parsePlays symbolToCard) input
    val = fromMaybe undefined res

testFilename = "day7-test.txt"

filename = "day7.txt"

testInput = "32T3K 765\nT55J5 684\nKK677 28\nKTJJT 220\nQQQJA 483"

day7 solver filename = do
  contents <- readFile filename
  print $ solver contents
