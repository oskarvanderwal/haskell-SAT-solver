module Solvers.SatBasics where
    
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Char
import System.IO
import Control.Applicative ((<|>))
import Data.List
import System.Environment
import Data.Map (fromListWith, toList)
--import Debug.Trace (trace)

-------------------------------------------------------------------------------------
-- FUNCTIONS MISSING IN CURRENT GHC VERSION
-------------------------------------------------------------------------------------
set_drop :: (Ord a) => Int -> Set a -> Set a
set_drop i = Set.fromList . (drop i) . Set.toList

-------------------------------------------------------------------------------------
-- DATA TYPES
-------------------------------------------------------------------------------------
-- data format for propositional expressions
data Expr =
    Const Bool
    | Var [Int]
    | Not Expr
    | And Expr Expr
    | Or Expr Expr
    | Implies Expr Expr
    | ImpliesBi Expr Expr -- iff
    deriving(Show, Eq, Ord)

-- types for combining expressions
type Clause     = Set Expr -- Format of disjunction of literals in Cnf (use only Var and Not Var as Expr)
type Cnf        = [Clause] -- Problem format
type Assignment = [Expr]   -- Solution format

-- data type of simplified formula with partial assignment (later, possibly more)
data Partial = Partial {formula :: Cnf,
                        assignment :: Assignment}
    deriving(Show, Eq)

-------------------------------------------------------------------------------------
-- CONVERT PROPOSITIONAL EXPRESSION TO CNF REPRESENTATION
-------------------------------------------------------------------------------------
-- Convert any propositional expression to the Conjunctive Normal Form
cnf :: Expr -> Cnf
cnf e = map (Set.fromList) (convertCnf . distrOr . distrNot . substOps $ e)

-- Substitutes And/Or/Not forms for Implies and ImpliesBi operations
substOps :: Expr -> Expr
substOps e =
    case e of
        -- substitution and recurse on sub-terms
        Implies x y   -> Not (substOps x) `Or` (substOps y)
        ImpliesBi x y -> (Not (substOps x) `Or` (substOps y)) `And` ((substOps x) `Or` Not (substOps y))
        -- recurse on sub-terms
        Not x         -> Not (substOps x)
        And x y       -> And (substOps x) (substOps y)
        Or x y        -> Or (substOps x) (substOps y)
        x             -> x

-- Distributes Not over the variables
distrNot :: Expr -> Expr
distrNot e =
    case e of
        -- double negations
        Not (Not x)   -> distrNot x
        -- constants
        Not (Const x) -> Const $ not x
        -- De Morgan's laws
        Not (And x y) -> (distrNot $ Not x) `Or` (distrNot $ Not y)
        Not (Or x y)  -> (distrNot $ Not x) `And` (distrNot $ Not y)
        -- recurse on sub-terms
        And x y       -> And (distrNot x) (distrNot y)
        Or x y        -> Or (distrNot x) (distrNot y)
        -- remainder
        x             -> x

-- Distribute Or over And to achieve the CNF
distrOr :: Expr -> Expr
distrOr e =
    -- repeat until there is no more change
    if e == e'
    then e
    else distrOr e'
    where 
        e' = distrOr' e
        distrOr' e =
            case e of
                -- distribute
                x `Or` (y `And` z) -> (x' `Or` (distrOr' y)) `And` (x' `Or` (distrOr' z))
                    where x' = distrOr' x
                (x `And` y) `Or` z -> ((distrOr' x) `Or` z') `And` ((distrOr' y) `Or` z')
                    where z' = distrOr' z
                -- recurse on sub-terms
                x `And` y          -> (distrOr' x) `And` (distrOr' y)
                x `Or` y           -> (distrOr' x) `Or` (distrOr' y)
                Not x              -> Not $ distrOr' x
                x                  -> x
                
-- Convert a propositional logic formula in CNF with type Expr to a list of lists of disjunction variables
convertCnf :: Expr -> [[Expr]]
convertCnf e =
    case e of
        And x y -> (convertCnf x) ++ (convertCnf y)
        Or x y  -> [(convOr x) ++ (convOr y)]
        x       -> [[x]]
    where
        convOr (Or x y) = (convOr x) ++ (convOr y)
        convOr x = [x]

-------------------------------------------------------------------------------------
-- STANDARD OPERATIONS ON CNF
-------------------------------------------------------------------------------------
-- perform unit propagation
unitPropagation :: Partial -> Partial
unitPropagation (Partial c a)
    | units == []  = Partial c a
    | otherwise    = unitPropagation (Partial (foldl substLiteral c units) (a++units))
    where
        units = unitClauses c

-- Returns a list of variables or their negations that occur as unit clause in a CNF
unitClauses :: Cnf -> [Expr]
unitClauses xs = [Set.elemAt 0 x | x <- xs, Set.size x == 1]

-- removes all sets which are tautologies
removeTautologies :: Partial -> Partial
removeTautologies (Partial c a) = Partial (filter (isNotTautology) c) a

-- returns True if the clause is not a tautology
isNotTautology :: Set Expr -> Bool
isNotTautology clause = Set.null $ Set.intersection negatives positives
  where
   negatives = Set.map makePositive $ Set.filter isNegative clause
   positives = Set.difference clause (Set.filter isNegative clause)

-- returns True if the expression is a negation
isNegative :: Expr -> Bool
isNegative v =
  case v of
    Var x       -> False
    Not (Var x) -> True
    x           -> error $ "Failed to check if variable is negative, not variable: " ++ show x

-- makes variable positive if it was negative
makePositive :: Expr -> Expr
makePositive v =
    case v of
        Var x -> Var x
        Not (Var x) -> Var x
        x           -> error $ "Failed to make variable positive, not variable: " ++ show x

-- makes variable negative if it was positive
makeNegative :: Expr -> Expr
makeNegative v =
    case v of
        Var x -> Not $ Var x
        Not (Var x) -> Not $ Var x
        x           -> error $ "Failed to make variable positive, not variable: " ++ show x

-- Substitutes 'True' for a literal (Var or Not Var) in a CNF
-- i.e. if the literal becomes true the clause is removed,
-- if it becomes false the literal is removed from the clause
substLiteral :: Cnf -> Expr -> Cnf
substLiteral cs v =
    case v of
        Var x       -> subst cs (Var x) True
        Not (Var x) -> subst cs (Var x) False
        y           -> error $ "Expression should be variable or its negation: " ++ show y
    where
        subst :: Cnf -> Expr -> Bool -> Cnf
        subst [] _ _ = []
        subst (c:cs) v b =
            case b of
                True  -> if Set.member v c
                         -- variable in clause, remove clause from CNF
                         then subst cs v b
                         -- otherwise, remove negated variable if in set
                         else c Set.\\ (Set.fromList [Not v]) : subst cs v b
                False -> if Set.member (Not v) c
                         -- negated variable in clause, remove clause from CNF
                         then subst cs v b
                         -- otherwise, remove positive variable if in set
                         else c Set.\\ (Set.fromList [v]) : subst cs v b

-- Remove pure literals
assignPureLiterals :: Partial -> Partial
assignPureLiterals p
  | null units = p
  | otherwise = (Partial (foldl substLiteral (formula p) units) ((assignment p)++units))
  where
    units = map (makeRightSign $ formula p) (findPureLiterals $ formula p)

-- Helper function for pure literals
makeRightSign c x
  | Set.member x (Set.unions c) = x
  | otherwise = Not x

-- returns a list of pure literals
findPureLiterals :: Cnf -> [Expr]
findPureLiterals c = Set.toList $ Set.union (Set.map makeNegative differences_neg_pos) differences_pos_neg
  where
    differences_neg_pos = Set.difference negatives positives
    differences_pos_neg = Set.difference positives negatives
    negatives = Set.map makePositive $ Set.filter isNegative $ Set.unions c
    positives = Set.difference (Set.unions c) $ Set.filter isNegative $ Set.unions c



-- Checks if CNF is unsatisfiable, i.e. if it has an empty clause
hasEmptyClause :: Cnf -> Bool
hasEmptyClause [] = False
hasEmptyClause (c:cs) = Set.size c == 0 || hasEmptyClause cs

-- Checks if CNF is satisfied, i.e. if it has no clauses left
hasNoClauses :: Cnf -> Bool
hasNoClauses c = length c == 0

-- Finds the frequency of each element in a list
frequency :: (Ord a) => [a] -> [(a, Int)]
frequency xs = toList (fromListWith (+) [(x, 1) | x <- xs])
