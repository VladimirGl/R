
massCenterVector <- function(v) {
  a <- c(1:length(v))
  
  round(sum(a * v) / sum(v))
}

massCenterString <- function(s) {
  v <- match(substring(s, 1:nchar(s), 1:nchar(s)), c(letters, LETTERS))
  
  print(v)
  v <- v %% (length(letters) + 1)
  print(v)
  
  massCenterVector(v)
}

