## ------------------------------------------------------------------------
knapsack_objects <-
data.frame(
  w=sample(1:4000, size = 2000, replace = TRUE),
  v=runif(n = 2000, 0, 10000))

## ------------------------------------------------------------------------
brute_force_knapsack <- function(x,W){
  
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
  
  bestCombinationList <- list(value = maximumValidValue, elements = validCombination)
  return(bestCombinationList)
}

## ----echo=FALSE,results="hide"-------------------------------------------
ptm <- proc.time()
brute_force_knapsack(x = knapsack_objects[1:16,], W = 3500)
result <- proc.time() - ptm

## ----echo=FALSE----------------------------------------------------------
result

## ------------------------------------------------------------------------
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
        combinationMatrix[i, j] <- max(val[i - 1] + tal,  combinationMatrix[i - 1, j])
      } else{
        combinationMatrix[i, j] <- combinationMatrix[i-1, j]
      }

    }
  }


  i <- nrow(x) + 1
  j <- W + 1
  n <- 1

  while (i >= 2 && j >= 1) {
    if (combinationMatrix[i, j] > combinationMatrix[i - 1, j]) {
      elements[n] <- el_order[i - 1]
      n <- n + 1
      j <- j - wt[i - 1]
    }
    i <- i - 1
  }

  list_ret <- list(value = round(max(combinationMatrix)), elements = sort(elements))
  return(list_ret)
}

## ----echo=FALSE,results="hide"-------------------------------------------
ptm <- proc.time()
knapsack_dynamic(x = knapsack_objects[1:500,], W = 3500)
result <- proc.time() - ptm

## ----echo=FALSE----------------------------------------------------------
result

## ------------------------------------------------------------------------
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

## ----echo=FALSE,results="hide"-------------------------------------------
ptm <- proc.time()
greedy_knapsack(x = knapsack_objects[1:1000000,], W = 3500)
result <- proc.time() - ptm

## ----echo=FALSE----------------------------------------------------------
result

