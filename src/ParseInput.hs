module ParseInput(parseInput) where
import Text.Parsec
import Text.Parsec.String
import Data.Char
import Control.Monad

import Types
import ParseUtil

parseInput :: Parser (Maybe MinFunction,[Constraint])
parseInput = do
  parseHeader
  skipMany comment
  mf <- optionMaybe minFunction
  skipMany comment
  cs <- many (skipMany comment >> constraint)
  skipMany comment
  eof
  return (mf, cs)

constraint = do
  optional spaces
  ts <- terms
  optional spaces
  o  <- operator
  optional spaces
  r  <- rhs
  optional spaces
  char ';'
  spaces
  return (ts `o` r)

rhs = number


operator = do
  (string ">=" >> return Geq)
  <|>
  (char '=' >> return Eq)

minFunction = do
  string "min:"
  optional spaces
  ts <- terms
  optional spaces
  char ';'
  newline
  return ts

parseHeader = do
  char '*'
  spaces
  string "#variable"
  optional spaces
  char '='
  optional spaces
  number
  optional spaces
  string "#constraint"
  char '='
  optional spaces
  number
  spaces


comment = do
  char '*'
  manyTill anyToken newline

terms = term `sepEndBy1` spaces

term :: Parser Term
term = do
  a <- number
  space
  x <- variable
  return (MkTrm a x)
  
