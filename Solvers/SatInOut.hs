module Solvers.SatInOut where
import System.IO
import System.Environment
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Char
import System.IO
import Control.Applicative ((<|>))
import Data.List
import System.Environment
import Debug.Trace (trace)
import Solvers.SatBasics

----------------------------------------------------------------------------------------------
-- READING
----------------------------------------------------------------------------------------------
-- Converts a DIMACS file (read as string) into a CNF list of sets of variables
readDimacs :: String -> Cnf
readDimacs contents = map (Set.fromList . readLine) allLines
  where
    allLines = filter (not . all isSpace) (tail . lines $ contents)
    readLine :: String -> [Expr]
    readLine line = map toVar (init . words $ line)
    toVar :: String -> Expr
    toVar x =
      if head x == '-'
      then Not (toVar $ tail x)
      else Var $ map digitToInt x
    
----------------------------------------------------------------------------------------------
-- WRITING
----------------------------------------------------------------------------------------------
-- writes all positive literals of a sudoku solution to DIMACS    
writeToDimacs :: Maybe Assignment -> String
writeToDimacs Nothing = "UNSAT"
writeToDimacs (Just a) = recursivePrint v
  where
    v = (Set.toList . Set.fromList . selectPositives $ a)
    --ch (Var [x,y,val]) = (show x)++(show y)++(show val)
    ch (Var e) = concat $ map (show) e
    recursivePrint [] = []
    recursivePrint (x:xs) = ((ch x)++" 0\n") ++ recursivePrint xs
    
-- writes a CNF in DIMACS format, set isLarge to True if size > 9
cnfToDimacs :: Bool -> Cnf -> String
cnfToDimacs l c = "\n" ++ cnfToDimacs' l c -- add empty line (officially needs to mention variable and clause count, e.g. "p cnf 999 12016")
    
cnfToDimacs' :: Bool -> Cnf -> String
cnfToDimacs' _ [] = ""
cnfToDimacs' isLarge (clause:cs) =
    (foldl1 (++) . map printVar . Set.toList $ clause) ++ "0\n" ++ cnfToDimacs' isLarge cs
    where
        printNumb x = if isLarge && x < 10 then "0"++show x else show x
        printVar (Not x) = '-' : printVar x
        printVar (Var x) = foldr (++) " " $ map printNumb x
        printVar x       = error $ "Not variable: " ++ show x

----------------------------------------------------------------------------------------------
-- PRINTING
----------------------------------------------------------------------------------------------
-- Select only the positive variables from an assignment
selectPositives :: Assignment -> Assignment
selectPositives [] = []
selectPositives (e:es) =
    case e of
        Var x -> Var x : selectPositives es
        x     -> selectPositives es

-- Select only the positive variables from a Maybe assignment
selectPositives' :: Maybe Assignment -> Assignment
selectPositives' Nothing = []
selectPositives' (Just a) = selectPositives a

-- Returns the solution to the sudoku as readable string
solutionPrint :: Maybe Assignment -> String
solutionPrint Nothing = "UNSAT"
solutionPrint (Just v) = recursivePrint v' 0
    where 
        v' = (Set.toList . Set.fromList . selectPositives $ v)
        ch (Var [x,y,val]) = intToDigit val
        recursivePrint [] _ = []
        recursivePrint (x:xs) n
            | n == 8    = ch x : ' ' : '\n' : recursivePrint xs 0
            | otherwise = ch x : ' ' : recursivePrint xs (n+1)
