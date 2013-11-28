
gcd <- function(a,b) ifelse (b==0, a, gcd(b, a %% b)) 

Rational <- function(a, b = 1) {
  if (b == 0) {
    warning("zero denominator")
    return(NA)
  }
  
  obj <- list(numerator = a, denominator = b)
  class(obj) <- "Rational"
  obj
}

reduce <- function(q) {
  UseMethod("reduce")
}

reduce.Rational <- function(q) {  
  reduceFactor <- gcd(q$numerator, q$denominator)
  
  Rational(q$numerator / reduceFactor, q$denominator / reduceFactor)
}

as.numeric.rational <- function(x) {
  reduce(Rational(x * 10e3, 10e3))
}

# В документации написано, что для S3 классов нужно определять as.double
as.double.Rational <- function(q) {
  q$numerator / q$denominator
}

print.Rational <- function(q) {
  cat(q$numerator)
  cat(" / ")
  cat(q$denominator)
}

'+.Rational' <- function(q1, q2 = Rational(0, 1)) {
  numerator <- q1$numerator * q2$denominator + q2$numerator * q1$denominator
  denominator <- q1$denominator * q2$denominator
  
  reduce(Rational(numerator, denominator))
}

'-.Rational' <- function(q1, q2 = Rational(0, 1)) {
  if (missing(q2)) {
    return(reduce(Rational(-q1$numerator, q1$denominator)))
  }
  
  q1 + (-q2)
}

'*.Rational' <- function(q1, q2) {
  reduce(Rational(q1$numerator * q2$numerator, q1$denominator * q2$denominator))
}

'/.Rational' <- function(q1, q2) {
  reduce(Rational(q1$numerator * q2$denominator, q1$denominator * q2$numerator))
}

'[.Rational' <- function(q, n) {
  if (n > 2 || n < 0) {
    warning("out of range")
    return(NA)
  }
  
  if (n == 1) {
    q$numerator
  } else {
    q$denominator
  }
}

'[<-.Rational' <- function(q, n, value) {
  if (n > 2 || n < 0) {
    warning("out of range")
    return(q)
  }
  
  if (n == 1) {
    Rational(value, q$denominator)
  } else {
    Rational(q$numerator, value)
  }
}

a <- Rational(1, 2)
b <- Rational(5, 6)
a + b
a - b
a * b
a / b
