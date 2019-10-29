import Solvers.SatBasics
import Solvers.SatInOut (cnfToDimacs)

import System.IO
import System.Environment
import Data.Set (Set)
import qualified Data.Set as Set

getIntArg :: IO Int
getIntArg = fmap (read . head) getArgs

-- generate a sudoku of given size as DIMACS
main = do
  size <- getIntArg
  putStrLn . cnfToDimacs (size > 9) . sudoku . fromIntegral $ size

-- make a disjunction of a set of clauses
disjunction :: [Expr] -> Expr
disjunction = foldl1 Or

-- make a conjunction of a set of clauses
conjunction :: [Expr] -> Expr
conjunction = foldl1 And

-- generate sudoku in CNF with a given size
sudoku :: Int -> Cnf
sudoku size = Set.toList . Set.fromList . cnf $
    -- at least one number per cell
    conjunction [disjunction [Var [x,y,val] | val <- [1..size]] | [x,y] <- [[x, y] | x <- [1..size], y <- [1..size]]]
    -- at most one number per cell
    `And` conjunction [conjunction [Var [x,y,val] `Implies` Not (disjunction [Var [x,y,val'] | val' <- [1..size], val' /= val]) | val <- [1..size]] | [x,y] <- [[x,y] | x <- [1..size], y <- [1..size]]]
    -- no number twice in a row
    `And` conjunction [conjunction [Var [x,y,val] `Implies` Not (disjunction [Var [x,y',val] | y' <- [1..size], y' /= y]) | y <- [1..size]] | [x,val] <- [[x,val] | x <- [1..size], val <- [1..size]]]
    -- no number twice in a column
    `And` conjunction [conjunction [Var [x,y,val] `Implies` Not (disjunction [Var [x',y,val] | x' <- [1..size], x' /= x]) | x <- [1..size]] | [y,val] <- [[y,val] | y <- [1..size], val <- [1..size]]]
    -- no number twice in a box
    `And` conjunction [conjunction [conjunction [Var [x,y,val] `Implies` Not (disjunction [Var [x',y',val] |
                                                [x',y'] <- [[xbox + i, ybox + j] | i <- [0..boxSize-1], j <- [0..boxSize-1]], 
                                                [x',y'] /= [x,y]])] | 
        val <- [1..size],
        [x,y] <- [[xbox + i, ybox + j] | i <- [0..boxSize-1], j <- [0..boxSize-1]]] |
            [xbox, ybox] <- [[xbox,ybox] | xbox <- [i*boxSize+1 | i <- [0..boxSize-1]], ybox <- [i*boxSize+1 | i <- [0..boxSize-1]]]]
    where
        boxSize = floor . sqrt . fromIntegral $ size
