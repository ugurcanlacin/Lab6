#' @title Greedy knapsack
#'
#' @description Greedy knapsack implementation
#'
#' @param x A data.frame cx with two variables v and w
#' @param W The weight capacity of the knapsack
#' @return The maximum knapsack value and corresponding elements that contributes to the value. 
#' 
#' @references \url{https://en.wikipedia.org/wiki/Knapsack_problem#Greedy_approximation_algorithm}
#' @export
greedy_knapsack <- function(x, W){
  
  if(!is.data.frame(x) || W < 0){
    stop("errounous input")
  }
  
  x$frac <- x$v/x$w
  x <- x[order(x$frac, decreasing = TRUE), ]
  
  value <- vector("numeric")
  elements <- vector("numeric")
  
  weight <- 0
  value <- 0
  
  i <- 1
  while(weight + x[i,"w"] < W){
    value <- value + x[i,"v"] 
    weight <- weight + x[i,"w"]
    elements[i] <- as.numeric(rownames(x[i,]))
    i <- i + 1
    
  }
  
  res <- list(
    "value" = round(value,digits = 0),
    "elements" = elements
  )
  
  return(res)
  
}

# set.seed(42)
# n <- 16
# knapsack_objects <-
#   data.frame(
#     w=sample(1:4000, size = n, replace = TRUE),
#     v=runif(n = n, 0, 10000)
#   )
# 
# ptm <- proc.time()
# greedy_knapsack(x = knapsack_objects[1:1000000,], W = 3500)
# proc.time() - ptm
