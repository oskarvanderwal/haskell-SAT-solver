module Solvers.SimpleBohm
  ( dpll
  ) where

import Data.Set (Set)
import qualified Data.Set as Set
import Data.Char
import System.IO
import System.Environment
import Control.Applicative ((<|>))
--import Debug.Trace (trace)
import Data.List
import Data.List (sortBy)
import Data.Function (on)
import Data.Map (fromListWith, toList)

import Solvers.SatBasics
import Solvers.SatInOut

-- function for debugging purposes (prints value after each call of arg)
--trace' arg = trace (show arg) arg

-- Returns the literal according to the Simple Bohm heuristic
firstLiteral :: Cnf -> Expr
firstLiteral [] = error "Failed to find first literal: CNF is empty"
firstLiteral c = bohmSelect c
  
bohmScore :: Cnf -> [(Expr, Double)]
bohmScore c = [(v, score v)| v <- allVars]
  where
    sep = clauseSizeSep c
    scores v = map (\x -> bohmCount x v) sep
    score v = vectorLength $ scores v
    allVars = Set.toList $ Set.fromList $ map makePositive $ Set.toList $ Set.fromList $ concat $ map Set.toList c

-- Returns list of lists of clauses that have same length, in order of length
clauseSizeSep :: Cnf -> [[Set Expr]]
clauseSizeSep clauses = [ [clause | clause <- clauses, s == Set.size clause] | s <- [2..maxSize]]
    where maxSize = maximum [Set.size x | x <- clauses]


-- Compute vector length
vectorLength :: [Double] -> Double
vectorLength = sqrt . sum . map (^2)


-- Select variable with highest Bohm score
bohmSelect :: Cnf -> Expr
bohmSelect = snd . head . sort . map swap . bohmScore
  where
    swap (x,y) = (y,x)

bohmCount ::  [Set Expr] -> Expr -> Double
bohmCount xs x = maximum [positives, negatives]
  where
    positives = fromIntegral $ (length . filter (Set.member x)) xs
    negatives = fromIntegral $ (length . filter (Set.member (Not (x)))) xs

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
