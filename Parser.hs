{-# LANGUAGE LambdaCase #-}

module Parser where

import Control.Applicative
import Data.Char

newtype Parser a = Parser {runParser :: String -> Maybe (String, a)}

instance Functor Parser where
  fmap f (Parser p) = Parser $ \input -> do
    (input, x) <- p input
    Just (input, f x)

instance Applicative Parser where
  pure x = Parser $ \input -> Just (input, x)
  (<*>) (Parser p1) (Parser p2) = Parser $ \input -> do
    (input, f) <- p1 input
    (input, a) <- p2 input
    Just (input, f a)

instance Alternative Parser where
  empty = Parser $ const Nothing

  (<|>) (Parser p1) (Parser p2) = Parser $ \input -> p1 input <|> p2 input

instance Monad Parser where
  (>>=) (Parser p) f = Parser $ \input -> case p input of
    Nothing -> Nothing
    Just (input, x) -> runParser (f x) input

parseId :: Parser Char
parseId = Parser $ \case
  ch : chs -> Just (chs, ch)
  _ -> Nothing

parseChar :: Char -> Parser Char
parseChar x = Parser $ \case
  y : ys
    | y == x -> Just (ys, x)
  _ -> Nothing

parseString :: String -> Parser String
parseString = traverse parseChar

charRangeP :: Int -> Int -> Parser Char
charRangeP l r = foldr ((<|>) . parseChar) empty [chr ch | ch <- [l .. r]]

parseLetter :: Parser Char
parseLetter = charRangeP 65 90

parseDigit :: Parser Char
parseDigit = charRangeP 48 57

parseN :: Integer -> Parser a -> Parser [a]
parseN n p
  | n == 0 = pure []
  | otherwise =
      ( do
          head <- p
          tail <- parseN (n - 1) p
          return (head : tail)
      )
        <|> pure []

parseStar :: Parser a -> Parser [a]
parseStar p =
  ( do
      head <- p
      tail <- parseStar p
      return (head : tail)
  )
    <|> pure []

parseOrL :: [String] -> Parser String
parseOrL = foldl (\pc ch -> parseString ch <|> pc) empty

parseOrR :: [String] -> Parser String
parseOrR = foldr (\ch pc -> parseString ch <|> pc) empty

ignoreParse :: Parser b -> Parser String
ignoreParse = (<$) ""

parseStarPred :: (Char -> Bool) -> Parser String
parseStarPred pred = Parser $ \input ->
  let (token, rest) = span pred input
   in Just (rest, token)

parsePlus :: Parser a -> Parser [a]
parsePlus p = do
  head <- p
  tail <- parseStar p
  return (head : tail)

parseNumber :: Parser Integer
parseNumber = read <$> parsePlus parseDigit

skipWS :: Parser String
skipWS = ignoreParse $ parseStarPred isSpace

parseStarDelim :: Char -> Parser a -> Parser [a]
parseStarDelim del p =
  ( do
      head <- p
      skipWS
      tail <- parseStar parseDelimAndP
      return (head : tail)
  )
    <|> return []
  where
    parseDelimAndP =
      do
        parseChar del
        skipWS
        p
