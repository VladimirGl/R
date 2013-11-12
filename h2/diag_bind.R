
zeros <- function(nrow, ncol) {
  m <- matrix(0, nrow, ncol)
  m
}

diagBindTwo <- function(m1, m2) {
  m1Zeros <- zeros(nrow(m1), ncol(m2))
  m2Zeros <- zeros(nrow(m2), ncol(m1))
  
  diagMatrix <- rbind(cbind(m1, m1Zeros), cbind(m2Zeros, m2))

  diagMatrix
}

diagBind <- function(...) {
  mNumber <- nargs()
  mDots <- list(...)

  diagMatrix <- mDots[[1]]

  if (length(mDots) != 1) {
    for (i in 2:mNumber) {
      diagMatrix <- diagBindTwo(diagMatrix, mDots[[i]])
    }
  }

  diagMatrix
}
