
zeros <- function(nrow, ncol) {
  m <- matrix(0, nrow, ncol)
  return(m)
}

diagBindTwo <- function(m1, m2) {
  m1Zeros <- zeros(nrow(m1), ncol(m2))
  m2Zeros <- zeros(nrow(m2), ncol(m1))
  
  diagMatrix <- rbind(cbind(m1, m1Zeros), cbind(m2Zeros, m2))

  return(diagMatrix)
}

diagBind <- function(...) {
  mNumber <- nargs()
  mList <- list(...)

  diagMatrix <- mList[[1]]

  for (i in 2:mNumber) {
    diagMatrix <- diagBindTwo(diagMatrix, mList[[i]])
  }

  return(diagMatrix)
}

m1 <- matrix(1:9, 3, 3)
m2 <- matrix(1:16, 4, 4)
m3 <- matrix(9:1, 3, 3)
m4 <- matrix(1, 2, 2)

m5 <- diagBind(m1, m2, m3, m4)
m5
