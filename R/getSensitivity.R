# Helper function
# Column 1,2,3,10: character
# Column 4-9: numeric
fill <- function(ll) {
  ll <- lapply(ll, function(x) x[2:length(x)])
  ll[[2]] <- c(rep(NA,4), ll[[2]])
  ll[[4]] <- c(rep(NA,4), ll[[4]])
  ll <- data.frame(matrix(unlist(ll), nrow=4, byrow=T), stringsAsFactors = FALSE)
  ll <- lapply(ll, function(x) gsub("^\\.$", "0", x))
  ll <- as.data.frame(ll, stringsAsFactors = FALSE)
  ll[,4:9] <- sapply(ll[,4:9], as.numeric)
  return(ll)
}

# Parse sensitivity report into a list of 2 data.frames.
#    1) constraintsRHS:
#             sensitivity report on change of constraint eqn RHS
#    2) objCoeff:
#             sensitivity report on change of  objective coefficients
getSensitivity <- function(fileName) {
  solData <- readLines(fileName)
  id1 <- c(9,10,12,13)
  df1 <- strsplit(solData[id1], "\\s+")
  df1 <- fill(df1)

  id2 <- c(23,24,26,27)
  df2 <- strsplit(solData[id2], "\\s+")
  df2 <- fill(df2)

  return(list(constraintsRHS = df1, objCoeff = df2))
}



