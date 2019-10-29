module Solvers.DLCS
  ( dpll
  ) where

import Data.Set (Set)
import qualified Data.Set as Set
import Data.Char
import System.IO
import System.Environment
import Control.Applicative ((<|>))
import Debug.Trace (trace)
import Data.List
import Data.List (sortBy)
import Data.Function (on)
import Data.Map (fromListWith, toList)

import Solvers.SatBasics
import Solvers.SatInOut

-- Returns the literal chosen by the DLCS heuristic
firstLiteral :: Cnf -> Expr
firstLiteral [] = error "Failed to find first literal: CNF is empty"
firstLiteral c  = mostPrevalentPolarity (fst . head $ findDLCS c) c
  where
    mostPrevalentPolarity :: Expr -> Cnf -> Expr
    mostPrevalentPolarity e c
      | totalNegatives < totalPositives = e
      | otherwise = Not e
        where
          totalNegatives = countExpression (Not e) (concat $ map Set.toList c)
          totalPositives = countExpression e (concat $ map Set.toList c)

-- Function to count how often an expression occurs in a list
countExpression :: Expr -> [Expr] -> Int
countExpression _ [] = 0
countExpression x xs = (length . filter (== x)) xs

sortDLCS :: Ord b => [(a, b)] -> [(a, b)]
sortDLCS = sortBy (flip compare `on` snd)

-- Find all (sorted) variables and their DLIS score (given bij Cp + Cn)
findDLCS :: Cnf -> [(Expr,Int)]
findDLCS c = sortDLCS $ frequency $ map makePositive $ concat $ map Set.toList c

-- Perform DPLL
dpll :: Partial -> Maybe Assignment
dpll p
    | hasEmptyClause . formula $ prop = Nothing
    | hasNoClauses . formula $ prop   = Just $ assignment prop
    | isNegative chosenLit            = dpll guessFalse <|> dpll guessTrue
    | otherwise                       = dpll guessTrue <|> dpll guessFalse
    where
        prop = unitPropagation p
        chosenLit = firstLiteral . formula $ prop --TODO: check what if first clause is empty
        firstLit = makePositive chosenLit
        guessTrue  = Partial (substLiteral (formula prop) firstLit) (assignment prop ++ [firstLit])
        guessFalse = Partial (substLiteral (formula prop) (Not firstLit)) (assignment prop ++ [Not firstLit])
