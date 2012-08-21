{-# LANGUAGE DeriveDataTypeable #-}
import Text.Parsec.String
import System.Console.CmdArgs
import Data.Maybe
import Control.Monad

import ParseInput
import ParseOutput
import Checker
import Types

data PBCompOutputChecker = App {
   inputFile :: FilePath,
   outputFile :: FilePath} deriving (Show,Data,Typeable)

pbCompOutputChecker = App { inputFile = def &= argPos 0 &= typ "INPUTFILE",
            outputFile = def &= argPos 1 &= typ "OUTPUTFILE"}
            &= summary "Checks that the output from the solver is correct for the input opb file"

main = do
  a <- cmdArgs pbCompOutputChecker 
  Right (mf,constr) <- parseFromFile parseInput (inputFile a)
  Right (res,ass) <- parseFromFile parseOutput (outputFile a) 
  let assig = makeAssignment ass
  print res
  when (res == Sat || res == OptFound) $
    do 
      forM constr (\c ->
        when (not (check assig c))
          (putStr "Unsatisfied constraint: " >> print constr))
      when (isJust mf)
        (putStr "Value of Objective Function: " >> print (valueOfMinFunction assig (fromJust mf)))
  
