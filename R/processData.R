processData <- function(idata){
  nVar <- NROW(idata$O)

  if ("B" %in% names(idata)) {
    idata$B$dir <- as.character(idata$B$dir)
    idata$A <- dfToMatrix(idata$A, nVar)
  } else {
    idata$B <- data.frame(
      dir = as.character(idata$A$dir),
      rhs = idata$A$rhs
    )
    var_col <- !names(idata$A) %in% c("dir", "rhs")
    idata$A <- dfToMatrix(idata$A[, var_col], nVar)
  }

  if ("Q" %in% names(idata)) {
    idata$Q <- dfToMatrix(idata$Q, nVar)
  }

  lb <- rep(0, nVar)
  if ('lb' %in% names(idata$O)){
    lb[!is.na(idata$O$lb)] = idata$O$lb[!is.na(idata$O$lb)]
  }
  idata$O$lb = lb

  ub <- rep(Inf, nVar)
  if ('ub' %in% names(idata$O)){
    ub[!is.na(idata$O$ub)] <- idata$O$ub[!is.na(idata$O$ub)]
  }
  idata$O$ub = ub


  if (!is.null(idata$O$type)){
    idata$O$type <- as.character(idata$O$type)
  }
  idata
}

# Read matrix data from optional inputs
# This function should get moved into the macro as it relates to getting
# data from Alteryx into R. This will also allow the package to NOT be
# dependent on AlteryxRDataX
# getMatrixData <- function(inputs){
#   read.Alteryx = getFromNamespace('read.Alteryx', "AlteryxRDataX")
#   if (inputs$inputMode == "matrix"){
#     idata = lapply(paste0('#', 1:3), read.Alteryx)
#     names(idata) = c("O", "A", "B")
#     if (inputs$problemType == "qp"){
#       idata$Q = read.Alteryx("#4")
#     }
#   } else {
#     idata = NULL
#   }
#   return(idata)
# }

# Code to Parse Model for Reporting ------
# Convert idata to df for parsing.
# INCOMPLETE
idata_to_df <- function(idata){
  df = as.vector(idata$O$coefficient)
  names(df) = as.character(idata$O$variable)
}

#' Convert input data.frame to matrix based on the matrix's format
#'
#'
#' @param df data frame
#' @param numCol number of columns
#' @return
#'   1. if the df has 'i','j','v' as columns name -- slam
#'   2. if the df doesn't -- dense matrix
#' @keywords internal
dfToMatrix <- function(df, numCol) {
  if (identical(names(df), c('i', 'j', 'v'))) {
    m <- slam::simple_triplet_matrix(
      i = df[,'i'],
      j = df[,'j'],
      v = df[,'v'],
      nrow = max(df[,'i']),
      ncol = numCol,
      dimnames = NULL
    )
    m <- fixSlamMatrix(m)
  } else {
    m <- as.simple_triplet_matrix(df)
  }
  m
}
