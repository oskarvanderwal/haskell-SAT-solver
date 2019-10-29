#!/bin/sh
# # chmod +x system_info.sh

i=0
while read p; do
    #echo "$i"
    cat ./Sudokus/sudoku-rules.txt > "./Data/sudoku$i.txt"
    ./sudoku_to_dimacs ./Sudokus/'1000 sudokus.txt' "$i" >> "./Data/sudoku$i.txt"
    true $((i=i+1))
done <./Sudokus/'1000 sudokus.txt'
