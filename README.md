# 4KR
This is the repository for the first project of the course Knowledge Representation (2018/2019, period 4), given at the Vrije Universiteit (VU). The goal of the project was to learn more about propositional logic by implementing our own SAT solver. The freedom of the project allowed us to combine this project with our own learning goal, namely to get experience with the functional programming language Haskell.

Note that this is a cleaned version, and that some parts of the original code are omitted for the purpose of clarity. For example, the output will not be in DIMACS format, and we have left out the code for running and analysing the experiments.

## How to run

To compile, use:

`ghc --make SAT.hs -O`

To run the SAT solver, run the following command:
`./SAT -Sn inputfile.txt`

Where -Sn stands for a specific DPLL strategy. The strategies are:

| Heuristic    | Command Line Argument |
|--------------|-----------------------|
| No heuristic | -S1                   |
| DLCS-min-min | -S2                   |
| Bőhm         | -S3                   |
| DLCS-min-max | -S4                   |
| DLCS-max-max | -S5                   |
| DLCS-max-min | -S6                   |
| Simple Bőhm  | -S7                   |

The `inputfile.txt` is a sudoku in DIMACS format. You can find these in the `Data` folder.

You can run for example:

`./SAT -S3 ./Data/sudoku1.txt`

The output should be the filled out sudoku:

```
2 4 9 1 8 6 5 7 3 
7 3 5 9 4 2 1 8 6 
1 6 8 3 7 5 4 2 9 
5 1 2 6 9 7 3 4 8 
9 7 6 8 3 4 2 5 1 
4 8 3 2 5 1 9 6 7 
6 9 4 7 2 3 8 1 5 
3 2 7 5 1 8 6 9 4 
8 5 1 4 6 9 7 3 2
```

## How to convert sudoku to DIMACS format
`make_all_sudokus.sh` is used to convert the textfile `Sudokus/1000 sudokus.txt`, with a sudoku on each line, to the DIMACS format. Make sure you run `ghc --make sudoku_to_dimacs.hs -O` to compile the required code. Use `sh make_all_sudokus.sh` to convert the example sudokus. The results can be found in `/Data/` and you can run the SAT solver on the converted sudokus.
