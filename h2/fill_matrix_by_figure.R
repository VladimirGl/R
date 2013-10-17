FillMatrixCircle <- function(nrow, ncol, x, y, R) {
	m <- matrix(FALSE, nrow, ncol)
  
	x.index <- x + R * cos(seq(0, 2*pi, length = 360))
	y.index <- y + R * sin(seq(0, 2*pi, length = 360))

	for (i in seq(x.index)) {
		m[x.index[i], y.index[i]] <- TRUE
	}

	return(m)
}

FillMatrixLine <- function(m, x1, y1, x2, y2) {
	x.index <- seq(x1, x2, length = max(dim(m)));
	y.index <- seq(y1, y2, length = max(dim(m)));

	for (i in seq(x.index)) {
		m[x.index[i], y.index[i]] <- TRUE
	}

	return(m)
}

FillMatrixTriangle <- function(nrow, ncol, x1, y1, x2, y2, x3, y3) {
	m <- matrix(FALSE, nrow, ncol)

	m <- FillMatrixLine(m, x1, y1, x2, y2)
	m <- FillMatrixLine(m, x1, y1, x3, y3)
	m <- FillMatrixLine(m, x2, y2, x3, y3)

	return(m)
}

FindSquareByDiagonal <- function(x1, y1, x2, y2) {
	centerVect <- c((x1 + x2) / 2, (y1 + y2) / 2)

	vectA <- c(x1, y1) - centerVect
	vectC <- -vectA

	transformMatrix <- matrix(c(0, 1, -1, 0), 2, 2)

	vectD <- as.vector(transformMatrix %*% vectA)
	vectB <- -vectD

	vectList <- list("B" = (vectB + centerVect), "D" = (vectD + centerVect))

	return(vectList)
}

FillMatrixSquare <- function(nrow, ncol, x1, y1, x2, y2) {
	m <- matrix(FALSE, nrow, ncol)

	foundCoords <- FindSquareByDiagonal(x1, y1, x2, y2)
	x3 <- foundCoords$B[1]
	y3 <- foundCoords$B[2]
	x4 <- foundCoords$D[1]
	y4 <- foundCoords$D[2]

	m <- FillMatrixLine(m, x1, y1, x3, y3)
	m <- FillMatrixLine(m, x3, y3, x2, y2)
	m <- FillMatrixLine(m, x2, y2, x4, y4)
	m <- FillMatrixLine(m, x4, y4, x1, y1)

	return(m)
}