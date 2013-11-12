n <- 7

maxEigen <- function(m) {
  if (ncol(m) != nrow(m)) {
    print("matrix is not square")
    return()
  }
  
  if (!isSymmetric(m)) {
    print("matrix is not symmetric")
    return()
   }

  myEps <- sqrt(.Machine$double.eps)
  x <- rnorm(nrow(m))

  vect <- m %*% x
  vOne <- (vect) / norm(vect)

  vect <- m %*% vect
  vTwo <- (vect) / norm(vect)

  while (norm(vTwo - vOne) > myEps) {    
    vOne <- vTwo

    vect <- m %*% vect
    vTwo <- (vect) / norm(vect)
  }

  eigenVect <- vOne
  eigenVal <- ((m %*% eigenVect) / eigenVect)

  eigenList <- list("vect" = as.vector(eigenVect), "val" = mean(eigenVal))

  eigenList
}

a <- matrix(rnorm(n^2), n, n)
a <- a + t(a)
a

eigen <- maxEigen(a)
eigen$vect
eigen$val
