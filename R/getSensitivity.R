getBlockLines <- function(solData) {
  page_starts <- grep(" - SENSITIVITY ANALYSIS REPORT", solData)
  page_ends <- grep("End of report", solData)
  data_starts <- page_starts + 8
  data_ends <- c(page_starts[2] - 1, page_ends - 1)
  return(list(row = c(data_starts[1], data_ends[1]),
              col = c(data_starts[2], data_ends[2])))
}

getDataLines <- function(start_end) {
  start <- start_end[1]
  end <- start_end[2]

  aa <- start:end
  aa <- subset(aa, (aa-start+1)%%3 != 0)
  return(aa)
}

# sensitivityHelper <- function(block_start, block_end) {
#   ind <- getDataLines(block_start, block_end)
#
# }

fillNAs <- function(ll) {
  n <- length(ll)
  inds <- 1:n
  inds <- subset(inds, inds %% 2 == 0)
  ll[inds] <- lapply(ll[inds], function(x) c(rep(NA,4), x))

  # Fix lines with "Limiting variable" empty
  short_inds <- which(sapply(ll, function(x) length(x) < 10))
  if (length(short_inds) > 0) {
    ll[short_inds] <- lapply(ll[short_inds], function(x) c(x, NA))
  }

  return(ll)
}

# Helper function
# Column 1,2,3,10: character
# Column 4-9: numeric
fill <- function(ll) {
  ll <- lapply(ll, function(x) x[2:length(x)])
  ll <- fillNAs(ll)

  ll <- data.frame(matrix(unlist(ll), nrow=length(ll), byrow=T),
                   stringsAsFactors = FALSE)
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
  block_lines <- getBlockLines(solData)
  ind1 <- getDataLines(block_lines$row)
  ind2 <- getDataLines(block_lines$col)

  df1 <- strsplit(solData[ind1], "\\s+")
  df1 <- fill(df1)
  df2 <- strsplit(solData[ind2], "\\s+")
  df2 <- fill(df2)

  return(list(constraintsRHS = df1, objCoeff = df2))
}



