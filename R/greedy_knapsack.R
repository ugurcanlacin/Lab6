#' @title Greedy knapsack
#' @description Calculates best values and weights for given capacity.
#' @param x Dataframe with variables v and w.
#' @param W Integer as capacity. 
#' @return The Function Returns maximum knapsack value and corresponding elements that contributes to the value. 
#' @references \url{https://en.wikipedia.org/wiki/Knapsack_problem#Greedy_approximation_algorithm}
#' @export
greedy_knapsack <- function(x, W){
  
  if(!is.data.frame(x) || W < 0){
    stop("errounous input")
  }
  
  x$frac <- x$v/x$w
  x <- x[order(x$frac,decreasing=TRUE), ]
  
  elements <- vector("numeric")
  value <- vector("numeric")
  
  value <- 0
  weight <- 0
  
  i <- 1
  
  while(weight + x$w[i] < W){
    value <- value + x$v[i]
    weight <- weight + x$w[i]
    elements[i] <- as.numeric(rownames(x[i,]))
    i <- i + 1
  }
  
  result <- list(value=value, elements=elements)
  
  return(result)
  
}

# set.seed(42)
# n <- 16
# knapsack_objects <-
#   data.frame(
#     w=sample(1:4000, size = n, replace = TRUE),
#     v=runif(n = n, 0, 10000)
#   )
# library(lineprof)
# greedy <- lineprof(greedy_knapsack(x = knapsack_objects[1:11,], W = 3500))
# shine(greedy)
# ptm <- proc.time()
# greedy_knapsack(x = knapsack_objects[1:1000000,], W = 3500)
# proc.time() - ptm
