detectMatrixType <- function(df){
  if (identical(names(df), c('i', 'j', 'v'))){
    'slam'
  } else if ('variable' %in% names(df)){
    'dense_variable'
  } else {
    'dense_constraint'
  }
}

factor2char <- function(df){
  i <- sapply(df, is.factor)
  df[i] <- lapply(df[i], as.character)
  return(df)
}

df2matVar <- function(inputs){
  matCols <- !(names(inputs$A) %in% c("variable"))
  matRows <- !(inputs$A$variable %in% c("dir", "description", "rhs"))
  if (!("B" %in% names(inputs))) {
    A <- t(sapply(inputs$A[matRows, matCols, drop = F], as.numeric))
    colnames(A) <- inputs$A$variable[matRows]
    B <- t(inputs$A[!matRows,,drop = F])
    B2 <- B[-1,]
    colnames(B2) <- unname(B[1,])
    B2 <- factor2char(as.data.frame(B2))
    B2 <- cbind(constraint = rownames(B2), B2)
    rownames(B2) <- NULL
    B2$rhs <- as.numeric(B2$rhs)
    inputs$A <- A
    inputs$B <- factor2char(B2)
  } else {
    A <- t(sapply(inputs$A[matRows, matCols, drop = F], as.numeric))
    colnames(A) <- inputs$A$variable[matRows]
    B <- cbind(
      constraints = setdiff(colnames(inputs$A), 'variable'),
      inputs$B
    )
    inputs$A <- A
    inputs$B <- factor2char(B)
  }
  inputs$A <- as.simple_triplet_matrix(inputs$A)
  return(inputs)
}

df2matCon <- function(inputs){
  matCols <- setdiff(names(inputs$A), c("constraint", "dir", "rhs", "description"))
  if (!("B" %in% names(inputs))) {
    if (!('constraint' %in% names(inputs$A))){
      inputs$A <- cbind(constraint = paste0("C", 1:NROW(inputs$A)), inputs$A)
    }
    B <- inputs$A[,!names(inputs$A) %in% matCols, drop = F]
    A <- as.matrix(inputs$A[,matCols,drop = F])
    rownames(A) <- inputs$A$constraint
    inputs$A <- A
    inputs$B <- factor2char(B)
  }
  inputs$A <- as.simple_triplet_matrix(inputs$A)
  inputs
}


df2matSlam <- function(df, numCol, ...){
  if (identical(names(df), c('i', 'j', 'v'))) {
    m <- slam::simple_triplet_matrix(
      i = df[,'i'],
      j = df[,'j'],
      v = df[,'v'],
      nrow = max(df[,'i']),
      ncol = numCol,
      dimnames = NULL
    )
    AlteryxPrescriptive:::fixSlamMatrix(m)
  }
}

fixMatrixO <- function(O){
  nVar = NROW(O)
  lb <- rep(0, nVar)
  if ('lb' %in% names(O)){
    lb[!is.na(O$lb)] = O$lb[!is.na(O$lb)]
  }
  O$lb = lb

  ub <- rep(Inf, nVar)
  if ('ub' %in% names(O)){
    ub[!is.na(O$ub)] <- O$ub[!is.na(O$ub)]
  }
  O$ub = ub

  if (!is.null(O$type)){
    O$type <- as.character(O$type)
  }
  O
}

processData <- function(inputs){
  matType <- detectMatrixType(inputs$A)
  inputs$O <- fixMatrixO(inputs$O)
  nVar = NROW(inputs$O)
  if (matType == "slam"){
    inputs$A <- df2matSlam(inputs$A, nVar)
  } else if (matType == "dense_variable") {
    inputs <- df2matVar(inputs)
  } else {
    inputs <- df2matCon(inputs)
  }

  if ("Q" %in% names(inputs)) {
    inputs$Q <- dfToMatrix(inputs$Q, nVar)
  }
  inputs$B$dir <- as.character(inputs$B$dir)
  return(inputs)
}
