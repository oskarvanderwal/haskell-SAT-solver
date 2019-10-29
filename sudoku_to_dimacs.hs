import System.IO
import System.Environment
import Data.Char

main = do
  args <- getArgs
  handle <- openFile (head args) ReadMode
  contents <- hGetContents handle
  putStrLn $ translateToDimacs (lines contents) (read $ last args)
  
-- Convert a sudoku from a line in a .txt file to a string in DIMACS format
txtToDimacs :: String -> String
txtToDimacs txt = 
    foldl1 (++)                                                           -- concat strings
    . map ((++ " 0\n") . foldl1 (++) . map printNumb)                     -- list to strings of a line ending in 0\n
    . filter ((/=0) . last)                                               -- filter empty spaces (dots, i.e. zeros)
    . zipWith (\x y -> x ++ [y]) [[x,y] | x <- [1..size], y <- [1..size]] -- integer to list of position and integer
    . map (\x -> if isHexDigit x then digitToInt x else 0)                -- character to integer (0 if dot)
    $ txt
    where
        size = floor . sqrt . fromIntegral . length $ txt
        printNumb x = if size > 9 && x < 10 then "0"++show x else show x

translateToDimacs :: [String] -> Int -> String
translateToDimacs sudokus line = 
    txtToDimacs $ sudokus !! line
