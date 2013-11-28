
swapply <- function(v, L, FUN, ...) {
  mList <- list()
  for (i in 1:(length(v) - L + 1)) {
    mList[[length(mList) + 1]] <- v[i:(i + L - 1)]
  }
  
  for (i in 1:length(mList)) {
    mList[[i]] <- FUN(mList[[i]], ...)
  }
  
  mList
}

swsapply <- function(v, L, FUN, ...) {
  mList <- swapply(v, L, FUN, ...)
  
  sizeVect <- length(mList[1:length(mList)])
  isMatrix <- FALSE
  isVector <- FALSE
  
  if ((length(unique(sizeVect))) == 1) {
    if (sizeVect[1] == 1) {
      return(as.vector(as.matrix(mList)))
    }
  } else {
    warning("cant do matrix or vector")
    return(mList)
  }

  if (isMatrix) {
    as.matrix(mList)
  }

  as.matrix(mList)
}

