module Checker where
import Types
import qualified Data.Map as M
import Data.Maybe

check :: Assignment -> Constraint -> Bool
check as (Geq l r) = sumUp as l >= r
check as (Eq  l r) = sumUp as l == r

valueOfMinFunction :: Assignment -> MinFunction -> Integer
valueOfMinFunction = sumUp

sumUp as = foldl (\y (MkTrm a x) -> if fromMaybe False $ M.lookup x as then a + y else y) 0
