module ParseUtil where
import Text.Parsec
import Text.Parsec.String
import Types
import Data.Char

number :: Parser Integer
number = 
  (char '-' >> (nat >>= return . negate))
  <|>
  (optional (char '+') >> nat)

nat :: Parser Integer
nat = do
  digits <- many1 digit
  return (foldl (\x d -> 10*x + toInteger (digitToInt d)) 0 digits)

variable :: Parser Var
variable = do
  char 'x'
  n <- number
  return (X n)
