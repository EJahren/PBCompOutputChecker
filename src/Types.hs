module Types where
import qualified Data.Map as M

data Result = Sat | Unsat | Unknown | OptFound deriving (Eq,Show)
data Lit = PosLit Var | NegLit Var deriving (Eq,Show)

data Constraint = Geq Lhs Rhs | Eq Lhs Rhs deriving (Show,Eq)
data Term = MkTrm Integer Var deriving (Show,Eq)

type Assignment = M.Map Var Bool

makeAssignment :: [Lit] -> Assignment
makeAssignment = foldl go M.empty
  where
  go as (PosLit x) = M.insert x True as
  go as (NegLit x) = M.insert x False as
                            
 
data Var = X Integer deriving (Eq,Show,Ord)
type Rhs = Integer
type Lhs = [Term]
type MinFunction = [Term]
