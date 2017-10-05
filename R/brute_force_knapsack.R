#'@title Brute Force Knapsack Algorithm
#'@description Calculates best values and weights for given capacity.
#'@param x Dataframe with variables v and w.
#'@param W Integer as capacity. 
#'@export
#'@return Returns a list with best value and combination.

brute_force_knapsack <- function(x,W){
  
  if(!is.data.frame(x) || W < 0){
    stop("errounous input")
  }
  
  listOfCombinations <- list()
  for(i in 1:nrow(x)){
    listOfCombinations[[i]] <- combn(rownames(x), i, paste, collapse = " ")
  }
  
  listOfWeights <- list()
  for(i in 1:nrow(x)){
    listOfWeights[[i]] <- combn(x$w, i,sum)
  }
  
  listOfValues <- list()
  for(i in 1:nrow(x)){
    listOfValues[[i]] <- combn(x$v, i,sum)
  }

  vectorOfCombinations <- unlist(listOfCombinations)
  vectorOfWeights <- unlist(listOfWeights)
  vectorOfValues <- round(unlist(listOfValues),0)
  
  weightsIndexesUnderTheCapacity <- which(vectorOfWeights < W)
  validValues <- vectorOfValues[weightsIndexesUnderTheCapacity]
  maximumValidValue <- max(validValues)
  
  validValuesIndexesInCombinationsVector <- which(vectorOfValues == maximumValidValue)
  validCombination <- vectorOfCombinations[validValuesIndexesInCombinationsVector]
  
  bestCombinationList <- list(value = maximumValidValue, elements = as.numeric(strsplit(validCombination, " ")[[1]]))
  return(bestCombinationList)
}

# set.seed(42)
# n <- 16
# knapsack_objects <-
#   data.frame(
#     w=sample(1:4000, size = n, replace = TRUE),
#     v=runif(n = n, 0, 10000)
#   )

# Start the clock!
# ptm <- proc.time()
# brute_force_knapsack(x = knapsack_objects[1:16,], W = 3500)
# Stop the clock
# proc.time() - ptm

