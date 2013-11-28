# что-бы было видно, точки должны лежать в промежутке от 0 до 1

Curve <- function(vx, vy) {
  vx <- vx[!is.na(vx)]
  vy <- vy[!is.na(vy)]
  
  if (length(vx) != length(vy)) {
    warning("bad data")
    return(NA)
  }
  
  sz <- length(vx)
  mCoords <- cbind(vx, vy)
  mTCoords <- rbind(mCoords[2:sz,], mCoords[1,])
  
  mCs <- mCoords + mTCoords / 6 - rbind(mCoords[sz,], mCoords[1:(sz - 1),]) / 6
  mDs <- mTCoords - rbind(mTCoords[2:sz,], mTCoords[1,]) / 6 + rbind(mTCoords[sz,], mTCoords[1:(sz - 1),]) / 6
    
  obj <- cbind(mCoords, mCs, mDs, mTCoords)
  class(obj) <- "Curve"
  obj
}

CurveGrid <- function(vx, vy) {
  obj <- list()
  obj[[1]] <- Curve(vx, vy)
  
  class(obj) <- "CurveGrid"
  
  obj
}

grid.CurveGrid <- function(g) {
  for (i in 1:length(g)) {
    for (j in 1:dim(g[[i]])[1]) {
      
      x <- c(g[[i]][j, seq(1, 8, by = 2)])
      y <- c(g[[i]][j, seq(2, 8, by = 2)])
      
      grid.bezier(x, y)
    }
  }
}

'+.CurveGrid' <- function(g1, g2) {
  for (i in 1:length(g2)) {
    g1[[length(g1) + 1]] <- g2[[i]]
  }
  
  g1
}