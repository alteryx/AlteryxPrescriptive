#' Generic function.
#'
#' @param x x
#' @param ... other arguments
#' @return Returns an object of class OP that is supported by the ROI class
#' @export
constructModel <- function(x, ...){
  UseMethod("constructModel")
}

# Construct objective function from payload
constructObjective <- function(x, ...){
  if (x$problemType == "Q"){
    if (x$solver == "gurobi"){

    }
  } else {

  }
  return(objective)
}

# Construct model file -----
constructModel.file_mode <- function(x, ...){
  x <- x$config
  mod <- readModelFile(x$filePath, type = x$fileType)
  constraints <- do.call(L_constraint, mod$constraints)
  if ("Q" %in% names(mod$objective)){
    objective <- Q_objective(
      Q = mod$objective$Q, L = as.vector(mod$objective$c)
    )
  } else {
    objective <- L_objective(as.vector(mod$objective))
  }
  OP(
    objective = objective,
    constraints = constraints,
    types = mod$types,
    bounds = getBounds(mod),
    maximum = mod$maximum
  )
}

# If the manual input UI creates a temp file in the CPLEX_LP format, and returns a path to it
#' @export
constructModel.manual_mode <- constructModel.file_mode

#' @export
constructModel.matrix_mode <- function(x, ...){
  idata <- processData(x$inputs)
  if ("Q" %in% names(idata)){
    objective <- Q_objective(Q = idata$Q, L = idata$O$coefficient)
  } else {
    objective <- as.vector(idata$O$coefficient)
  }
  constraints <- L_constraint(
    L = idata$A,
    dir = idata$B$dir,
    rhs = idata$B$rhs
  )
  OP(
    objective = objective,
    constraints = constraints,
    types = idata$O$type,
    bounds = getBounds_matrix(idata),
    maximum = x$config$maximize
  )
}
