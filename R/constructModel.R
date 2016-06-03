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
  objective
}

# Construct model file -----
#' @export
constructModel.file_mode <- function(x, ...){
  checkInputs_file_mode(x)
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
  op_obj <- OP(
    objective = objective,
    constraints = constraints,
    types = mod$types,
    bounds = getBounds(mod),
    maximum = mod$maximum
  )

  list(OP = op_obj, OPAttributes = attributes(mod))
}

# If the manual input UI creates a temp file in the CPLEX_LP format, and returns a path to it
#' @export
constructModel.manual_mode <- function(x, ...){
  #checkInputs_manual_mode(x)
  manualInput = Filter(Negate(is.null), list(
    constraints = x$config$constraints,
    objective = x$config$objective,
    fieldList = x$config$fieldList,
    maximize = x$config$maximize
  ))

  tf = tempfile(fileext = ".lp")
  cplex = makeCPLEXFile(manualInput)
  cat(cplex)
  cat(cplex, file = tf)
  x$config$inputMode = "file"
  x$config$filePath = tf
  x$config$fileType = 'CPLEX_LP'
  print(tf)
  class(x) <- c(class(x)[1], paste0(x$config$inputMode, "_mode"))
  constructModel(x)
}

#' @export
constructModel.matrix_mode <- function(x, ...){
  idata <- processData(x$inputs)
  checkInputs_matrix_mode(idata)
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
  op_obj <- OP(
    objective = objective,
    constraints = constraints,
    # types = idata$O$type,
    bounds = getBounds_matrix(idata),
    maximum = x$config$maximize
  )

  list (OP = op_obj, OPAttributes = getOPAttributes(idata))
}

## Helper function: get optimization problem attributes for matrix input mode
getOPAttributes <- function(x) {
  num_obj <- getNumObjective(x$O)

  list(
    n_objective_vars = num_obj$total,
    n_integer_vars = num_obj$n_integer,
    n_binary_vars = num_obj$n_binary,
    n_constraints = nrow(x$B),
    n_nonzeros = length(x$A$i),
    problem_name = "",
    objective_name = "",
    objective_vars_names = as.character(x$O$variable),
    constraint_names = addConstraintNames(nrow(x$A), x$B$constraint)
  )
}

addConstraintNames <- function(nConstraints, cNames = NULL){
  if (is.null(cNames)){
    paste0("C", 1:nConstraints)
  } else {
    as.character(cNames)
  }
}

getNumObjective <- function(y) {
  x <- y$type
  if (is.null(x)){
    list(
      total = NROW(y),
      n_integer = 0,
      n_binary = 0
    )
  } else {
    temp <- table(x)
    n_integer <- if (is.na(temp['I'])) 0 else temp[['I']]
    n_binary  <- if (is.na(temp['B'])) 0 else temp[['B']]

    list(
      total = length(x),
      n_integer = n_integer + n_binary,
      n_binary = n_binary
    )
  }
}
