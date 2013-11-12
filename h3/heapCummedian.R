library(proto)

heap <- proto()

heap$list <- c()
heap$size <- function(.) {
  length(.$list)
}

heap$comparision <- function(., x, y) {}

heap$add <- function(., value) {
  .$list <- append(.$list, value)
  
  i <- .$size()
  parent <- (i - 1) %/% 2
  
  if (parent == 0) {
    parent <- 1
  }
  
  while (i > 1 & .$comparision(.$list[parent], .$list[i])) {
    temp <- .$list[i]
    .$list[i] <- .$list[parent]
    .$list[parent] <- temp
    
    i <- parent
    parent <- (i - 1) %/% 2
    
    if (parent == 0) {
      parent <- 1
    }
  }
}

heap$heapify <- function(., node) {
  leftChild <- 0
  rightChild <- 0
  largestChild <- 0
  
  while (TRUE) {
    leftChild <- 2 * node
    rightChild <- 2 * node + 1
    significantChild <- node
    
    if (leftChild < .$size() & .$comparision(.$list[significantChild], .$list[leftChild])) {
      significantChild <- leftChild
    }
    
    if (rightChild < .$size() & .$comparision(.$list[significantChild], .$list[rightChild])) {
      significantChild <- rightChild
    }
    
    if (significantChild == node) {
      break
    }
    
    temp <- .$list[node]
    .$list[node] <- .$list[significantChild]
    .$list[significantChild] <- temp
    node <- significantChild
  }
}

heap$getTop <- function(.) {
  result <- .$list[1]
  
  .$list[1] <- .$list[.$size()]
  .$list <- head(.$list, -1)
  .$heapify(1)
  result
}

heap$top <- function(.) {
  .$list[1]
}

maxHeap <- proto(heap)
maxHeap$comparision <- function(., x, y) {
  x < y
}

minHeap <- proto(heap)
minHeap$comparision <- function(., x, y) {
  x > y
}

quantile <- proto()
quantile$q <- 0.5
quantile$max <- maxHeap
quantile$min <- minHeap

quantile$addValue <- function(., value) {
  if (.$max$size() == 0) {
    .$max$add(value)
    return()
  }
  
  if (.$min$size() == 0) {
    if (.$max$top() > value) {
      .$min$add(.$max$getTop())
      .$max$add(value)
    } else {
      .$min$add(value)
    }
    return()
  }
  
  if (.$max$top() >= value) {
    .$max$add(value)
  } else {
    .$min$add(value)
  }

  if (.$max$size() * .$q > .$min$size() * (1 - .$q)) {
    .$min$add(.$max$getTop())
  } else if (.$max$size() * .$q < .$min$size() * (1 - .$q)) {
    .$max$add(.$min$getTop())
  }
}

quantile$value <- function(.) {
  q <- (.$max$top() + .$min$top()) / 2
  
  if (.$max$size() * .$q > .$min$size() * (1 - .$q)) {
    q <- .$max$top()
  } else if (.$max$size() * .$q < .$min$size() * (1 - .$q)) {
    q <- .$min$top()
  }
  
  q
}

cumquantile <- function(vect, q) {
  quantile$q <- q
  quantile$min$list <- c()
  quantile$max$list <- c()
  
  cumVect <- c()
  
  for (i in 1:length(vect)) {
    quantile$addValue(vect[i])
    
    cumVect <- append(cumVect, quantile$value())
  }
  
  cumVect
}


