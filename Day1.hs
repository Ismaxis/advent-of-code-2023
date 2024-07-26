module Day1 where

import Control.Applicative
import Control.Monad
import Data.Char (digitToInt, isDigit)
import Data.List (intercalate)
import Data.Map qualified as Map
import GHC.Base (ord)
import GHC.Char (chr)
import Parser
import System.IO

digits = ["one", "two", "three", "four", "five", "six", "seven", "eight", "nine"]

reversedDigits = map reverse digits

digitsMap =
  Map.fromList list
  where
    list = aux [] digits [1 .. length digits]
    aux res _ [] = res
    aux res (literal : literalTail) (i : tail) = aux ([(literal, show i), (reverse literal, show i)] ++ res) literalTail tail

parseSpelledDigit digits = (Map.!) digitsMap <$> parseOrL digits

---

splitOn :: (Eq a) => a -> [a] -> [[a]]
splitOn ch s = reverse $ aux s 0 s []
  where
    aux rest _ [] res = rest : res
    aux rest i (head : tail) res
      | head == ch =
          let (word, _ : rest') = splitAt i rest
           in aux rest' 0 tail (word : res)
      | otherwise = aux rest (i + 1) tail res

getFirstDigit [] = 0
getFirstDigit (head : tail)
  | isDigit head = digitToInt head
  | otherwise = getFirstDigit tail
  where
    getLast cur [] = cur
    getLast cur (head : tail)
      | isDigit head = getLast head tail
      | otherwise = getLast cur tail

getLastDigit = getFirstDigit . reverse

getCalibrationValue s = 10 * getFirstDigit (translateFirst s) + getLastDigit (translateLast s)

translateFirstDigits digits input = let Just (rest, res) = runParser parser input in res ++ rest
  where
    parser =
      parseSpelledDigit digits
        <|> ( do
                head <- parseDigit <|> parseId
                tail <- parser
                return (head : tail)
            )
        <|> pure ""

translateFirst = translateFirstDigits digits

translateLast input = reverse $ translateFirstDigits reversedDigits (reverse input)

--- solve
solveDay1 s = sum [getCalibrationValue line | line <- splitOn '\n' s]

solveDay1_2 s = sum [getCalibrationValue line | line <- splitOn '\n' s]

day1_x solver filename = do
  contents <- readFile filename
  print $ solver contents

day1 = day1_x solveDay1

day1_2 = day1_x solveDay1_2
