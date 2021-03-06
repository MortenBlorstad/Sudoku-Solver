Sudoku Solver
================

A sudoku solver algorithm using backtracking. The sudoku board is solved
by assigning numbers to empty cells, one by one. Given the rules of
sudoku, we check whether the number can be placed in the empty cell
before assigned the number to the cell. If the number is valid we assign
the number, and recursively check if this assignment leads to a solution
or not. If it does not lead to a solution, we try the next number for
the current empty cell. If none of the numbers from 1 to 9 leads to a
solution, the is no solution to the board.

## Examples

``` r
# load the solver class
source("solver.R")
```

The boards to the left are the boards to solve. The boards to the right
are the completed boards.

``` r
board <- matrix(c(
  0,0,0,0,0,0,0,0,4,
  3,0,5,0,0,0,0,2,0,
  0,0,6,0,0,9,0,0,8,
  9,1,3,0,2,7,0,0,0,
  6,0,7,0,0,5,0,0,3,
  0,0,0,0,0,3,0,9,1,
  4,0,9,0,3,2,5,8,6,
  0,6,0,5,0,0,0,3,0,
  0,0,0,0,6,0,0,0,0
), nrow = 9, ncol=9, byrow = T)

# initilize 
ss = SudokuSolver$new(board)
# draw board 
ss$drawBoard()
# get the solution 
solution = ss$getSolution()
# draw completed board
ss$drawBoard()
```

<img src="README_files/figure-gfm/board-1.png" width="50%" /><img src="README_files/figure-gfm/board-2.png" width="50%" />

``` r
board2 <- matrix(
  c(0,0,0,0,0,6,0,0,0,
    0,9,5,7,0,0,3,0,0,
    4,0,0,0,9,2,0,0,5,
    7,6,4,0,0,0,0,0,3,
    0,0,0,0,0,0,0,0,0,
    2,0,0,0,0,0,9,7,1,
    5,0,0,2,1,0,0,0,9,
    0,0,7,0,0,5,4,8,0,
    0,0,0,8,0,0,0,0,0),
  byrow = T,
  ncol = 9
)


ss = SudokuSolver$new(board2)
ss$drawBoard()
solution = ss$getSolution()
ss$drawBoard()
```

<img src="README_files/figure-gfm/board2-1.png" width="50%" /><img src="README_files/figure-gfm/board2-2.png" width="50%" />

``` r
board3 <- matrix(
  c(3, 0, 6, 5, 0, 8, 4, 0, 0, 
    5, 2, 0, 0, 0, 0, 0, 0, 0, 
    0, 8, 7, 0, 0, 0, 0, 3, 1, 
    0, 0, 3, 0, 1, 0, 0, 8, 0, 
    9, 0, 0, 8, 6, 3, 0, 0, 5, 
    0, 5, 0, 0, 9, 0, 6, 0, 0, 
    1, 3, 0, 0, 0, 0, 2, 5, 0, 
    0, 0, 0, 0, 0, 0, 0, 7, 4, 
    0, 0, 5, 2, 0, 6, 3, 0, 0),
nrow = 9, ncol=9, byrow = T)



ss = SudokuSolver$new(board3)
ss$drawBoard()
solution = ss$getSolution()
ss$drawBoard()
```

<img src="README_files/figure-gfm/board3-1.png" width="50%" /><img src="README_files/figure-gfm/board3-2.png" width="50%" />
