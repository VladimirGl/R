
generateSymmetric <- function(n) {
  vect <- rnorm(n ^ 2)
  
  a <- matrix(vect, n)
  a +t(a)
}

myIsSymmetric <- function(m) {
  identical(m, t(m))
}

myIsSymmetric2 <- function(m) {
  isSymmetric(m)
}

myIsAntysymmetric <- function(m) {
  myIsSymmetric2(t(apply(m, 2, rev)))
}

