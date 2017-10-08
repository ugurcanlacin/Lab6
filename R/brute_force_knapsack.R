#'@title Brute Force Knapsack Algorithm
#'@description Calculates best values and weights for given capacity.
#'@param x Dataframe with variables v and w.
#'@param W Integer as capacity. 
#'@param parallel Boolean makes function parallel
#'@export
#'@return Returns a list with best value and combination.

brute_force_knapsack <- function(x,W, parallel = FALSE){
  
  if(!is.data.frame(x) || W < 0){
    stop("Errounous Input.")
  }
  if(parallel == FALSE){
      listOfCombinations <- list()
      for(i in 1:nrow(x))
      {
        listOfCombinations[[i]] <- combn(rownames(x), i, paste, collapse = " ")
      }
      
      listOfWeights <- list()
      for(i in 1:nrow(x))
      {
        listOfWeights[[i]] <- combn(x$w, i,sum)
      }
      
      listOfValues <- list()
      for(i in 1:nrow(x) )
      {
        listOfValues[[i]] <- combn(x$v, i, sum)
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
  }else{
    require(parallel)
    requireNamespace("parallel")
    
    numberOfCores <- detectCores() - 1
    cluster <- makeCluster(numberOfCores)
    clusterExport(cluster , c("x") ,envir = environment())
    
    listOfCombinations <- parLapplyLB(cluster, 1:nrow(x), fun =  function(y) {
      combn(rownames(x) , y , paste0, collapse = " ")
    })
    listOfWeights <- parLapplyLB(cluster, 1:nrow(x), fun =  function(y) {
      combn(x$w , y, sum)
    })
    listOfValues <- parLapplyLB(cluster,1:nrow(x), fun =  function(y) { 
      combn(x$v , y , sum)
    })
    
    stopCluster(cluster)
    
    vectorOfCombinations <- unlist(listOfCombinations)
    vectorOfWeights <- unlist(listOfWeights)
    vectorOfValues <- round(unlist(listOfValues),0)
    
    weightsIndexesUnderTheCapacity <- which(vectorOfWeights < W)
    validValues <- vectorOfValues[weightsIndexesUnderTheCapacity]
    maximumValidValue <- max(validValues)
    
    validValuesIndexesInCombinationsVector <- which(vectorOfValues == maximumValidValue)
    validCombination <- vectorOfCombinations[validValuesIndexesInCombinationsVector]
    
    bestCombinationList <- list(value = maximumValidValue, elements = as.numeric(strsplit(validCombination, " ")[[1]]))
  }
  return(bestCombinationList)
}
# set.seed(42)
# n <- 16
# knapsack_objects <-
#   data.frame(
#     w=sample(1:4000, size = n, replace = TRUE),
#     v=runif(n = n, 0, 10000)
#   )
# ptm <- proc.time()
# brute_force_knapsack(x = knapsack_objects[1:16,], W = 3500,parallel = TRUE)
# proc.time() - ptm
# library(lineprof)
# brute_normal <- lineprof(brute_force_knapsack(x = knapsack_objects[1:16,], W = 3500,parallel= TRUE))
# shine(brute_normal)
