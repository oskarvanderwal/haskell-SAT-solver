import Data.Set (Set)
import qualified Data.Set as Set
import System.IO
import System.Environment

import qualified Solvers.Noheuristic as Noheuristics
import qualified Solvers.DLCS as DLCS
import qualified Solvers.DLCS_least as DLCS_least
import qualified Solvers.DLCS_adapted as DLCS_adapted
import qualified Solvers.DLCS_adapted_least as DLCS_adapted_least
import qualified Solvers.SimpleBohm as SimpleBohm
import qualified Solvers.Bohm as Bohm

import Solvers.SatInOut
import Solvers.SatBasics

dispatch :: [(String, Partial -> Maybe Assignment)]
dispatch = [ ("-S1", noHeuristic)
           , ("-S2", dlcs_a_l)
           , ("-S3", bohm)
           , ("-S4", dlcs_a)
           , ("-S5", dlcs)
           , ("-S6", dlcs_l)
           , ("-S7", simplebohm)
           ]

-- Run compiled code as such: cat rules_and_sudoku.txt | ./SAT_solver
-- Can concatenate rules and sudoku as such:
-- cat sudoku-rules.txt > rules_and_sudoku.txt; cat sudoku-example.txt >> rules_and_sudoku.txt
main :: IO ()
main = do
   (command:filename) <- getArgs
   contents <- readFile (last filename)
   let (Just action) = lookup command dispatch
   putStrLn . solutionPrint . action $ (removeTautologies $ assignPureLiterals $ Partial (readDimacs contents) [])

noHeuristic :: Partial -> Maybe Assignment
noHeuristic = Noheuristics.dpll 

dlcs :: Partial -> Maybe Assignment
dlcs = DLCS.dpll

dlcs_a :: Partial -> Maybe Assignment
dlcs_a = DLCS_adapted.dpll

dlcs_l :: Partial -> Maybe Assignment
dlcs_l = DLCS_least.dpll

dlcs_a_l :: Partial -> Maybe Assignment
dlcs_a_l = DLCS_adapted_least.dpll

simplebohm :: Partial -> Maybe Assignment
simplebohm = SimpleBohm.dpll

bohm :: Partial -> Maybe Assignment
bohm = Bohm.dpll

