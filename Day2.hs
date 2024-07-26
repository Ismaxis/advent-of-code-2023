module Day2 where

import Data.Map qualified as M
import Parser

colors = ["blue", "red", "green"]

parseColor = parseOrL colors

parseCube = do
  count <- parseNumber
  skipWS
  color <- parseColor
  return (color, count)

parseTurn = M.fromList <$> parseStarDelim ',' parseCube

parseGame = do
  skipWS
  _ <- parseString "Game"
  skipWS
  id <- parseNumber
  _ <- parseChar ':'
  skipWS
  maps <- parseStarDelim ';' parseTurn
  return (id, foldr (M.unionWith max) M.empty maps)

isGamePossible gameLimits game = all (>= 0) $ M.unionWith (-) gameLimits game

parseGames = parseStar parseGame

gameLimits = M.fromList [("red", 12), ("green", 13), ("blue", 14)]

solve :: String -> Integer
solve input = case runParser parseGames input of
  Just (_, games) -> foldr ((+) . fst) 0 $ filter (isGamePossible gameLimits . snd) games
  _ -> -1

solve2 :: String -> Integer
solve2 input = case runParser parseGames input of
  Just (_, games) -> sum $ map (product . snd) games
  _ -> -1

testInput = "Game 1: 3 blue, 4 red; 1 red, 2 green, 6 blue; 2 green\nGame 2: 1 blue, 2 green; 3 green, 4 blue, 1 red; 1 green, 1 blue\nGame 3: 8 green, 6 blue, 20 red; 5 blue, 4 red, 13 green; 5 green, 1 red\nGame 4: 1 green, 3 red, 6 blue; 3 green, 6 red; 3 green, 15 blue, 14 red\nGame 5: 6 red, 1 blue, 3 green; 2 blue, 1 red, 2 green"

day2 solver filename = do
  contents <- readFile filename
  print $ solver contents
