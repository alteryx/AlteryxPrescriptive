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
  inputs$A <- factor2char(inputs$A)
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
  inputs$A[is.na(inputs$A)] <- 0
  inputs$A <- as.simple_triplet_matrix(inputs$A)
  return(inputs)
}

df2matCon <- function(inputs){
  # matCols: columns with numeric values, i.e. numbers in constraint equations
  matCols <- setdiff(names(inputs$A), c("constraint", "dir", "rhs", "description"))

  # Add constraint names "C1, C2 ..." if it doesn't exist in Input A
  if (!('constraint' %in% names(inputs$A))){
    inputs$A <- cbind(constraint = paste0("C", 1:NROW(inputs$A)), inputs$A)
  }
  if (!("B" %in% names(inputs))) {
  B <- inputs$A[,!names(inputs$A) %in% matCols, drop = F]
  inputs$B <- factor2char(B)
  }
  A <- as.matrix(inputs$A[,matCols,drop = F])
  rownames(A) <- inputs$A$constraint
  inputs$A <- A
  inputs$A[is.na(inputs$A)] <- 0
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
    fixSlamMatrix(m)
  }
}

fixMatrixO <- function(O){
  nVar = NROW(O)
  lb <- rep(0, nVar)
  if ('lb' %in% names(O)){
    lb[!is.na(O$lb)] = O$lb[!is.na(O$lb)]
  }
  O$lb = as.numeric(lb)

  ub <- rep(Inf, nVar)
  if ('ub' %in% names(O)){
    ub[!is.na(O$ub)] <- O$ub[!is.na(O$ub)]
  }
  O$ub = as.numeric(ub)

  if (!is.null(O$type)){
    O$type <- as.character(O$type)
  }
  O
}

#' Infer column names for Input O to "variable", "coefficient", "lb", "ub", "type"
#'
#' @param O A data.frame, original O matrix from input Anchor O.
#' @param config configuration object as a list.
inferO <- function(O, config) {
  displayFlag <- config$displayFieldMapO
  r <- c("variable", "coefficient", "lb", "ub", "type")
  names(r) <- config[c("nameVar", "nameCoef", "nameLower", "nameUpper", "nameType")]
  if (displayFlag) {
    O <- plyr::rename(O, r)
  }
  return(O)
}

#' Infer column names for Input A based on constraintMode
#'
#' @param A A data.frame, original A matrix from input Anchor A
#' @param constrMode A string, constraint mode.
inferA <- function(A, constrMode) {
  if (constrMode == 'conInRow') {
    constraint <- names(Filter(function(x){return(!isTRUE(x))},
                               sapply(A, is.numeric)))
    # if (is.null(constraint) || length(constraint) == 0) {
    #   stop("Error: lack of constraint column in Input A.")
    # } else
    if (length(constraint) > 1) {
      stop("Error: there shouldn't be any other string type of columns except constraint.")
    }
    names(A)[names(A) == constraint] <- 'constraint'
  }else if (constrMode == 'varInRow') {
    variable <- names(Filter(function(x){return(!isTRUE(x))},
                             sapply(A, is.numeric)))
    if (is.null(variable) || length(variable) == 0) {
      stop("Error: lack of variable column in Input A.")
    } else if (length(variable) > 1) {
      stop("Error: there shouldn't be any other string type of columns except variable.")
    }
    names(A)[names(A) == variable] <- 'variable'
  }
  return(A)
}

#' Infer column names for Input B to 'constraint', 'rhs', 'dir'
#'
#' @param B A data.frame, original B matrix from input Anchor B
#' @examples
#' \dontrun{
#'  B <- data.frame(
#'   z = c('A', 'B', 'C'),
#'   x = c(">=", "<=", "=="),
#'   y = c(1, 2, 3)
#'  )
#'  inferB(B)
#' }
inferB <- function(B) {
  n = ncol(B)
  # infer rhs
  rhs <- names(Filter(isTRUE, sapply(B, is.numeric)))
  if (is.null(rhs)) {
    stop("Error: the rhs column should be of type numeric.")
  }

  # infer dir
  if (n > 3) {
    stop("Error: Input B should only have constraint, dir, rhs columns")
  } else if (n == 3) {
    dir <- names(Filter(
      function(x){all(x %in% c(">=", "<=", "==", ">", "<", "="))},
      lapply(B[,names(B) != rhs], unique)
    ))
  } else if (n == 2) {
    if (all(levels(B[, names(B) != rhs]) %in% c(">=", "<=", "==", ">", "<", "="))) {
      dir <- names(B)[!(names(B) %in% rhs)]
    }
  } else {
    stop("Error: Input B should at least dir and rhs columns")
  }
  if (is.null(dir)) {
    stop("Error: the dir column should have >=, <=, ==, > or <.")
  }

  # infer constraint
  if (n == 3) {
    constraint <- names(B)[!(names(B) %in% c(rhs, dir))]
    repl <- c('constraint', 'rhs', 'dir')
    names(repl) <- c(constraint, rhs, dir)
  } else {
    repl <- c('rhs', 'dir')
    names(repl) <- c(rhs, dir)
  }

  # Rename
  plyr::rename(B, replace = repl)
}

processData <- function(inputs, config){
  if ("displayFieldMapO" %in% names(config) && config$displayFieldMapO) {
    inputs$O <- inferO(inputs$O, config)
  }
  if ("constraintMode" %in% names(config)) {
    inputs$A <- inferA(inputs$A, config$constraintMode)
  }
  if (!is.null(inputs$B)) inputs$B <- inferB(inputs$B)

  matType <- detectMatrixType(inputs$A)
  inputs <- lapply(inputs, factor2char)
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
