n <- 7

maxEigen <- function(m) {
  if (ncol(m) != nrow(m)) {
    print("matrix is not square")
    return()
  }

  myEps <- sqrt(.Machine$double.eps)
  an <- m
  x <- rnorm(nrow(m))

  vect <- an %*% x
  vOne <- (vect) / norm(vect)

  an <- an %*% m
  vect <- an %*% x
  vTwo <- (vect) / norm(vect)

  while (((norm(vTwo - vOne) > myEps)) & (!all(vTwo == 0))) {    
    vOne <- vTwo

    an <- an %*% m
    vect <- an %*% x
    vTwo <- (vect) / norm(vect)
  }

  eigenVect <- vOne
  eigenVal <- ((m %*% eigenVect) / eigenVect)

  eigenList <- list("vect" = as.vector(eigenVect), "val" = mean(eigenVal))

  return(eigenList)
}

a <- matrix(rnorm(n^2), n, n)
a <- a + t(a)
a

eigen <- maxEigen(a)
eigen$vect
eigen$val