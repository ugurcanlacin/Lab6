#'@title Dynamic Knapsack Algorithm
#'@description Calculates best values and weights for given capacity.
#'@param x Dataframe with variables v and w.
#'@param W Integer as capacity. 
#'@export
#'@return Returns a list with best value and combination.

knapsack_dynamic <- function(x,W){
  if(!is.data.frame(x) || W < 0){
    stop("errounous input")
  }
  
  # Creating empty matrix
  combinationMatrix <- matrix(NA, ncol = W + 1, nrow = nrow(x) + 1)
  combinationMatrix[1,] <- 0
  combinationMatrix[,1] <- 0 
  
  el_order <- order(x$w)
  
  wt <- x[order(x$w), 1]
  val <- x[order(x$w), 2]
  elements <- c()
  
  for (i in 1:(nrow(x) + 1)) {
    for (j in 1:(W + 1)) {
      if (i == 1 || j == 1) {
        combinationMatrix[i, j] <- 0
      } else if (wt[i - 1] < j - 1 | wt[i - 1] == j - 1) {
        if(combinationMatrix[i - 1, j - wt[i - 1]] == 0){
          tal <- 0
        } else {
          tal <- combinationMatrix[i - 1, j - wt[i - 1]]
        }
        combinationMatrix[i, j] <- max(val[i - 1]+tal,  combinationMatrix[i-1,j])
      } else{
        combinationMatrix[i, j] <- combinationMatrix[i-1, j]
      }
      
    }
  }
  
  
  i <- nrow(x)+1
  j <- W+1
  n <- 1
  
  while (i >= 2 && j >= 1) {
    if (combinationMatrix[i, j] > combinationMatrix[i-1,j]) {
      elements[n] <- el_order[i-1]
      n <- n + 1
      j <- j-wt[i-1]
    }
    i <- i-1
  }
  
  list_ret <- list(value = round(max(combinationMatrix)), elements = sort(elements))
  return(list_ret)
}

# set.seed(42)
# n <- 16
# knapsack_objects <-
#   data.frame(
#     w=sample(1:4000, size = n, replace = TRUE),
#     v=runif(n = n, 0, 10000)
#   )

# library(lineprof)
# dynamic <- lineprof(knapsack_dynamic(x = knapsack_objects[1:11,], W = 3500))
# shine(dynamic)
# ptm <- proc.time()
# knapsack_dynamic(x = knapsack_objects[1:8,], W = 3500)
# proc.time() - ptm

