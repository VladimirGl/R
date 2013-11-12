max <- 12345

sieveOfEratosthenes <- function(maxNumber) {
  arr <- rep(TRUE, maxNumber)
  arr[1] = FALSE

  maxSqrt <- sqrt(maxNumber)

  for (i in 2:maxSqrt) {
    if (arr[i] == TRUE) {
      if (i^2 < maxNumber) {
	arr[seq(i^2, maxNumber, by = i)] <- FALSE
      }
    }
  }

  arr
}

printPrimes <- function(arr) {
  print(which(arr == TRUE))
}

b <- sieveOfEratosthenes(max)
printPrimes(b)

