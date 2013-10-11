max <- 1000000

sieveOfEratosthenes <- function(maxNumber) {
  arr <- rep(T, maxNumber)

  maxSqrt <- sqrt(maxNumber)

  for (i in 2:maxSqrt) {
    if (arr[i] == T) {
      for (j in seq(i^2, maxNumber, by = i)) {
	arr[j] <- F
      }
    }
  }

  return(arr)
}

printPrimes <- function(arr, maxNumber) {
  for (i in 1:maxNumber) {
    if (arr[i] == T) {
      print(i)
    }
  }
}

b <- sieveOfEratosthenes(max)
printPrimes(b, max)