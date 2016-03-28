# Helper function
fill <- function(ll) {
  ll <- lapply(ll, function(x) x[2:length(x)])
  ll[[2]] <- c(rep(NA,4), ll[[2]])
  ll[[4]] <- c(rep(NA,4), ll[[4]])
  data.frame(matrix(unlist(ll), nrow=4, byrow=T))
}

# Parse sensitivity report into a list of 2 data.frames.
#    1) constraintsRHS:
#             sensitivity report on change of constraint eqn RHS
#    2) objCoeff:
#             sensitivity report on change of  objective coefficients
getSensitiviy <- function(fileName) {
  solData <- readLines(fileName)
  id1 <- c(9,10,12,13)
  df1 <- strsplit(solData[id1], "\\s+")
  df1 <- fill(df1)

  id2 <- c(23,24,26,27)
  df2 <- strsplit(solData[id2], "\\s+")
  df2 <- fill(df2)

  return(list(constraintsRHS = df1, objCoeff = df2))
}



