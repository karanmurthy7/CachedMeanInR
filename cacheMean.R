makeVector <- function(numbersList = numeric()){
  
  meanValue <- NULL
  
  get <- function()
    numbersList
  
  setMean <- function(valuePassedFromCacheMean)
    meanValue <<- valuePassedFromCacheMean
  
  getMean <- function()
    meanValue
  
  list( get = get, setMean = setMean, getMean = getMean)
}


cacheMean <- function(utility, ...){
  meanValue <- utility$getMean()
  if(!is.null(meanValue)){
      message("Returning cached mean value")
      return(meanValue)
  }
  
  number <- utility$get()
  meanValue <- mean(number, ...)
  utility$setMean(meanValue)
  meanValue
  
}