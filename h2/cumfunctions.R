Skewness <- function(vect) {
	skewness <- NaN

	if (length(vect) != 1) {
		skewness <- mean((vect - mean(vect)) ^ 3) / sd(vect)^3
	}

	skewness
}

Kurtosis <- function(vect) {
	kurtosis <- NaN
	
	if (length(vect) != 1) {
		kurtosis <- mean((vect - mean(vect)) ^ 4) / sd(vect)^4 - 3
	}
	
	kurtosis
}

Cumfunction <- function(vect, func) {
	cumFuncVector <- func(vect[1])

	for (i in 2:length(vect)) {
		cumFuncVector <- append(cumFuncVector, func(vect[1:i]))
	}

	cumFuncVector
}

Cummedian <- function(vect) {
	Cumfunction(vect, median)
}

Cumsd <- function(vect) {
	Cumfunction(vect, sd)
}

Cummean <- function(vect) {
	Cumfunction(vect, mean)
}

Cumskewness <- function(vect) {
	Cumfunction(vect, Skewness)
}

Cumkurtosis <- function(vect) {
	Cumfunction(vect, Kurtosis)
}
