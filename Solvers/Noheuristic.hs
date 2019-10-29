module Solvers.Noheuristic
  ( dpll
  ) where

import Data.Set (Set)
import qualified Data.Set as Set
import Data.Char
import System.IO
import System.Environment
import Control.Applicative ((<|>))
import Data.List
--import Debug.Trace (trace)
import Solvers.SatBasics
import Solvers.SatInOut

-- Returns the first literal of the first clause
firstLiteral :: Cnf -> Expr
firstLiteral [] = error "Failed to find first literal: CNF is empty"
firstLiteral c  = (Set.elemAt 0) . head $ c

-- Perform DPLL
dpll :: Partial -> Maybe Assignment
dpll p
    | hasEmptyClause . formula $ prop = Nothing
    | hasNoClauses . formula $ prop   = Just $ assignment prop
    | otherwise                       = dpll guessTrue <|> dpll guessFalse
    where
        prop = unitPropagation p
        firstLit = makePositive . firstLiteral . formula $ prop --TODO: check what if first clause is empty
        guessTrue  = Partial (substLiteral (formula prop) firstLit) (assignment prop ++ [firstLit])
        guessFalse = Partial (substLiteral (formula prop) (Not firstLit)) (assignment prop ++ [Not firstLit])
