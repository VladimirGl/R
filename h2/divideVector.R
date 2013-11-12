DivideVector <- function(vect, n) {
	split(vect, 1:n)
}

PositiveEqualSums <- function(vect, n) {
	sortedVec <- rev(sort(vect))
	
	size <- length(vect)
	
	vectList <- list()
	
	for (i in 1:n) {
		vectList[[i]] <- sortedVec[i]
	}
	
	for (j in (n + 1):size) {
		vectList[[n]] <- append(vectList[[n]], sortedVec[j])
		
		if (sum(vectList[[n]]) > sum(vectList[[n - 1]])) {
			count <- n
			
			while ((count > 2) & (sum(vectList[[count]]) > sum(vectList[[count - 1]]))) {
				temp <- vectList[count]
				vectList[count] <- vectList[count - 1]
				vectList[count - 1] <- temp
				
				count <- count - 1
			}
			
			if (sum(vectList[[1]]) < sum(vectList[[2]])) {
				temp <- vectList[1]
				vectList[1] <- vectList[2]
				vectList[2] <- temp
			}
		}
		
		j <- j + 1
	}
	
	vectList
}

# нормально работает для n << length(vect)
EqualSumVectors <- function(vect, n) {
	positiveVect <- vect[vect >= 0]
	negativeVect <- vect[vect < 0]

	# плохо разделять задачу таким образом, но при n << длины вектора, работает
	positiveList <- PositiveEqualSums(positiveVect, n)
	negativeList <- PositiveEqualSums(-negativeVect, n)
	
	finalList <- list()
	
	# можно тут еще не тупо i к i, а найти те, которые дадут наименьшее отклонение
	for (i in 1:n) {
		finalList[[i]] <- append(positiveList[[i]], -negativeList[[i]])
	}
	
	finalList
}

PrintSums <- function(vecList, n) {
	sumVec <- c(sum(vecList[[1]]))

	for (i in 2:n) {
		sumVec <- append(sumVec, sum(vecList[[i]]))
	}
	
	sumVec
}

a <- rnorm(1000)

b <- EqualSumVectors(a, 30)
d <- PrintSums(b, 30)
d

