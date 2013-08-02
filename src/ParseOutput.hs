module ParseOutput(parseOutput) where
import Text.Parsec
import Text.Parsec.String
import Text.Parsec.Perm
import Data.Char


import Types
import ParseUtil

parseOutput = do
  r <- permute (
    combine <$$>
      (skipMany unwanted >> result)
      <||>
      (skipMany unwanted >> solutionLine))
  skipMany unwanted
  eof
  return r
  where
    combine r1 r2 = (r1,concat r2)
    unwanted = comment <|> emptyLine <|> (objectiveValue >> return "")
  
      

--parseOutput = do
--  skipMany unwanted
--  r1 <- result
--  skipMany unwanted
--  r2 <- solutionLine
--  skipMany unwanted
--  eof
--  return (r1,concat r2)
--    where
--      combine r1 r2 = (r1,concat r2)
--      unwanted = comment <|> emptyLine <|> (objectiveValue >> return "")

solutionLine = many solution

emptyLine =
  manyTill space newline

comment = do
  char 'c'
  manyTill anyToken newline

objectiveValue :: Parser Integer
objectiveValue = do
  char 'o'
  spaces
  n <- number
  spaces
  return n

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
