module Parsing (
  parseLines,
  parseCommaSeparated,
  parseInt,
  resultOrError)
where

import Text.Parsec (parse, getInput, setInput, (<|>), sepEndBy, many1, eof)
import Text.Parsec.String (Parser)
import Text.Parsec.Char (string, letter, spaces)
import Control.Applicative (empty)
import Numeric (readDec, readSigned)
import Data.Bifunctor (first)

resultOrError :: Either String b -> b
resultOrError = either error id

parseLines :: Parser a -> String -> Either String [a]
parseLines = parseSeparated eol

parseCommaSeparated :: Parser a -> String -> Either String [a]
parseCommaSeparated = parseSeparated (string ",")

parseSeparated :: Parser String -> Parser a -> String -> Either String [a]
parseSeparated sep p s = first show $ parse (sepEndBy p sep) "" s

eol :: Parser String
eol = string "\r\n" <|> string "\n"

parseInt :: Parser Int
parseInt = do s <- getInput
              case readSigned readDec s of
                [(n, s')] -> n <$ setInput s'
                _         -> empty