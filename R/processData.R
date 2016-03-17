processData <- function(idata){

  nrow = NROW(idata$O)
  if (identical(names(idata$A)[1:3], c('i', 'j', 'v'))){
    idata$A <- as.list(idata$A)
    idata$A$nrow = max(idata$A$i)
    idata$A$ncol = NROW(idata$O)
    idata$A <-  structure(idata$A, class = 'simple_triplet_matrix')
    idata$A <- fixSlamMatrix(idata$A)
  } else {
    idata$A = as.matrix(idata$A)
  }

  if ("Q" %in% names(idata)) {
    if (identical(names(idata$Q)[1:3], c('i', 'j', 'v'))){
      idata$Q <- as.list(idata$Q)
      idata$Q$nrow = as.integer(max(idata$Q$i))
      idata$Q$ncol = NROW(idata$O)
      idata$Q <-  structure(idata$Q, class = 'simple_triplet_matrix')
      idata$Q <- fixSlamMatrix(idata$Q)
    } else {
      idata$Q = as.matrix(idata$Q)
    }
  }

  lb <- rep(0, nrow)
  if ('lb' %in% names(idata$O)){
    lb[!is.na(idata$O$lb)] = idata$O$lb[!is.na(idata$O$lb)]
  }
  idata$O$lb = lb

  ub <- rep(Inf, nrow)
  if ('ub' %in% names(idata$O)){
    ub[!is.na(idata$O$ub)] = idata$O$ub[!is.na(idata$O$ub)]
  }
  idata$O$ub = ub

  idata$B$dir = as.character(idata$B$dir)
  if (!is.null(idata$O$type)){
    idata$O$type = as.character(idata$O$type)
  }
  return(idata)
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
