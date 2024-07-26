module Day5 where

import Data.Foldable (asum)
import Data.Maybe (fromMaybe)
import Parser
import Text.XHtml (content)

testMap = "seed-to-soil map:\n50 98 2\n52 50 48"

parseSeeds = do
  skipWS
  parseString "seeds:"
  parseStar parseNumberStar
  where
    parseNumberStar = do
      skipWS
      parseNumber

newtype Range = Range (Integer, Integer) deriving (Show)

contains [] _ = False
contains ((Range (start, len)) : rs) val
  | start <= val && val < start + len = True
  | otherwise = contains rs val

parseSeedsRange :: Parser [Range]
parseSeedsRange = do
  skipWS
  parseString "seeds:"
  seeds <- parseStar parseNumberStar
  return $ getRanges seeds []
  where
    parseNumberStar = do
      skipWS
      parseNumber
    getRanges [] t = t
    getRanges (s : l : xs) t = getRanges xs (Range (s, l) : t)

data Mapping = Mapping Integer Integer Integer deriving (Show)

mapping :: Mapping -> Integer -> Maybe Integer
mapping (Mapping dest source len) val
  | source <= val && val < source + len = Just (dest + val - source)
  | otherwise = Nothing

revMapping (Mapping dest source len) = Mapping source dest len

newtype Map = Map [Mapping] deriving (Show)

(!) :: Map -> Integer -> Integer
(!) (Map mappings) val = fromMaybe val (asum $ map (`mapping` val) mappings)

rev (Map mappings) = Map $ map revMapping mappings

parseMap = do
  parseMapName
  parseChar ':'
  Map <$> parseStar parseMapping
  where
    parseMapName = parseStarPred (/= ':')
    parseMapping = do
      skipWS
      dest <- parseNumber
      skipWS
      source <- parseNumber
      skipWS
      Mapping dest source <$> parseNumber

data Almanac r = Almanac [r] [Map]

parseAlmanac seedParser = do
  seeds <- seedParser
  maps <- parseStar parseMap
  return (Almanac seeds maps)

almanacLookup maps seed = foldl (flip (!)) seed maps

solve input = case runParser (parseAlmanac parseSeeds) input of
  Just (_, Almanac seeds maps) -> minimum (map (almanacLookup maps) seeds)
  _ -> -1

solveRanges input = case runParser (parseAlmanac parseSeedsRange) input of
  Just (_, Almanac ranges maps) -> loop 0 ranges (reverse (map rev maps))
  _ -> -1
  where
    loop i ranges maps = if contains ranges $ almanacLookup maps i then i else loop (i + 1) ranges maps

testFilename = "day5-test.txt"

filename = "day5.txt"

day5 solver filename = do
  contents <- readFile filename
  print $ solver contents

-- > day5 solveRanes filename
-- > 79004094
