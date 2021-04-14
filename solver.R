
library(R6)
rm(list=ls())

SudokuSolver = R6Class("SudokuSolver", private = list(
          board = NA,
          solvedBoard = NA,
          findRange= function(x){
            return((3*x-2):(3*x))
          },
          isPossible = function(grid, row, col,n){
            if(any(grid[row,] == n))
              return(FALSE)
            if(any(grid[,col] == n))
              return(FALSE)
            
            cols = ceiling(col/3)
            rows = ceiling(row/3)
            if(any(grid[private$findRange(rows),private$findRange(cols)] == n))
              return(FALSE)
            
            return(TRUE)
            
          },
          find_empty_cells = function(board) {
            
            which(board == 0, arr.ind = TRUE)
            
          },
          solver = function(grid){
            
            ## find cell to fill
            needed_cells <- private$find_empty_cells(grid)
            
            
            if(nrow(needed_cells) ==0){ # if no cells to fill, we are done. (base case)
              private$solvedBoard = grid
              return(TRUE)
            }
            
            # the row and col of the first cell in the list.
            row <- needed_cells[1, 1] 
            col <- needed_cells[1, 2]
            
            # valid values of n are all digits from 1 to 9
            for(n in 1:9){
              if(private$isPossible(grid,row,col,n)){ # check if the value n can be a part of the solution
                
                # make tentative assignment
                grid[row, col] <- n
                if (private$solver(grid)){ # if success, return true
                  return(TRUE)
                }
                # else: failure, unmake & try again
                grid[row, col] <- n
              }
            }
            # this triggers backtracking 
            return(FALSE)
          }
        ) ,     
  public =list(
  initialize = function(board){
    stopifnot(is.matrix(board), nrow(board)==9, ncol(board) == 9)
    private$board = board
    private$solvedBoard = board
  },
  drawBoard = function(){
    cols=ncol(private$solvedBoard)
    row=nrow(private$solvedBoard)
    #draw canvas
    par(mar = c(.1, .1, .1, .1), xpd=TRUE)
    plot(NULL, xlim = c(0,row), ylim = c(0,cols),axes = FALSE, xlab = "", ylab="",asp =1)
    clip(0,cols,0,cols)
    
    # draw cells
    for (i in 0:cols) {
      abline(v=i,lwd =1)
      abline(h=i,lwd =1)
    }
    
    # draw subgrids
    for (i in seq(0,10,3)) {
      lines(x = rep(i,10), y = 0:9,lwd =3)
      lines(x =0:9 , y = rep(i,10),lwd =3)
    }
    # draw numbers
    for(i in 1:row){
      for(j in 1:row){
        num = private$solvedBoard[j,i]
        if(num ==0)
          num = ""
        text(i-0.5,j-0.5, num,cex = 1, col = 1 )
      }
    }
    
    
  },
  
  # solve the board and prints the solution
  getSolution = function(){
    if (private$solver(private$board)){
      return(private$solvedBoard)
    }else
      return("No Solution exists")
  }
)
)





