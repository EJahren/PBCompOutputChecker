module ParseOutput(parseOutput) where
import Text.Parsec
import Text.Parsec.String
import Data.Char


import Types
import ParseUtil

parseOutput = do
  skipMany comment
  r1 <- result
  skipMany comment
  r2 <- if r1 == Sat || r1 == OptFound then solution else return []
  skipMany comment
  eof
  return (r1,r2)


comment = do
  char 'c'
  manyTill anyToken newline

result :: Parser Result
result = do
  char 's'
  spaces
  result <- resultString 
  spaces
  return result

solution = do
  char 'v'
  optional spaces
  assignment <- literal `sepEndBy` spaces
  return assignment

literal =
  (char '-' >> (variable >>= return . NegLit))
  <|>
  (            (variable >>= return . PosLit))



resultString =
 (string "SATISFIABLE" >> return Sat)
 <|>
 (string "OPTIMUM FOUND" >> return OptFound)
 <|>
 (string "UNSATISFIABLE" >> return Unsat)
 <|>
 (string "UNKNOWN" >> return Unknown)
