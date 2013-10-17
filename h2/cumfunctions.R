Skewness <- function(vect) {
	vm <- vect - mean(vect)
	vsd <- sd(vect)

	skewness <- (sum(vm ^ 3) / n) / (vsd ^ 3)
	return(skewness)
}

Kurtosis <- function(vect) {
	vm <- vect - mean(vect)
	vsd <- sd(vect)

	kurtosis <- ((sum(vm ^ 4) / n) / (vsd ^ 4)) - 3
	return(kurtosis)
}

Cumfunction <- function(vect, func) {
	counter <- 1
	cumVector <- vect[1]

	cumFuncVector <- func(cumVector)

	for (i in 2:length(vect)) {
		cumVector <- append(cumVector, vect[i])
		cumFuncVector <- append(cumFuncVector, func(cumVector))
	}

	return(cumFuncVector)
}

Cummedian <- function(vect) {
	return(Cumfunction(vect, median))
}

Cumsd <- function(vect) {
	return(Cumfunction(vect, sd))
}

Cummean <- function(vect) {
	return(Cumfunction(vect, mean))
}

Cumskewness <- function(vect) {
	return(Cumfunction(vect, Skewness))
}

Cumkurtosis <- function(vect) {
	return(Cumfunction(vect, Kurtosis))
}