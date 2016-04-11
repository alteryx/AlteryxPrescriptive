#' Solve model
#'
#' @param x x, fron constructModel, ROI OP object
#' @param solver solver
#' @param ... additional arguments. currently not used.
#' @import ROI ROI.plugin.glpk ROI.plugin.quadprog quadprog glpkAPI
solveModel <- function(x, solver, ...){
  UseMethod('solveModel')
}

solveModel.default <- function(x, solver = 'glpk'){
  ROI::ROI_solve(x, solver = solver)
}

solveModel.glpkAPI <- function(x, solver = 'glpkAPI', lp_attr){
  solve_glpkAPI(x, lp_attr)
}

solveModel.gurobi <- function(x, solver = 'gurobi'){
  if (requireNamespace('gurobi')){
    solve_gurobi(x)
  }
}

solve_glpkAPI <- function(lp, attr) {
  prob <- initProbGLPK()
  setProbNameGLPK(prob, attr$objective_name)

  modelsense <- if (lp$maximum) GLP_MAX else GLP_MIN
  setObjDirGLPK(prob, modelsense)

  addRowsGLPK(prob, attr$n_constraints)
  addColsGLPK(prob, attr$n_objective_vars)

  setRowsNamesGLPK(prob, 1:attr$n_constraints, attr$constraint_names)
  setColsNamesGLPK(prob, 1:attr$n_objective_vars, attr$objective_vars_names)

  lb <- lp$constraints$rhs
  ub <- lb
  type <- lp$constraints$dir
  type[type == "<="] <- GLP_UP
  type[type == ">="] <- GLP_LO
  type[type == "=="] <- GLP_FX
  type <- as.numeric(type)
  setRowsBndsGLPK(prob, 1:attr$n_constraints, lb, ub, type)

  #Set the type and bounds of columns and the objective function using a function which
  #has the ability to work with vectors.
  bounds <- getBounds_glpkAPI(lp, attr$n_objective_vars)
  obj <- as.vector(terms(objective(lp))$L)
  setColsBndsObjCoefsGLPK(prob, 1:attr$n_objective_vars, bounds$lb, bounds$ub, obj, bounds$type)

  #Load the constraint matrix.
  mm <- constraints(lp)$L

  ia <- mm$i
  ja <- mm$j
  ar <- mm$v
  loadMatrixGLPK(prob, attr$n_nonzeros, ia, ja, ar)
  #Solve the problem using the simplex algorithm.
  invisible(solveSimplexGLPK(prob))

  printRangesGLPK(lp = prob, fname = "sensitivity_report.txt")
  df_sen <- getSensitivity("sensitivity_report.txt")

  return(df_sen)
}

#' Solve model using Gurobi -----
#'
#' @keywords internal
solve_gurobi <- function(lp) {
  bounds <- getBounds_gurobi(lp)
  Q_ <- terms(objective(lp))$Q
  model <- list(
    modelsense = if (lp$maximum) "max" else "min",
    obj = as.vector(terms(objective(lp))$L),
    Q = if (is.null(Q_)) NULL else Q_/2,
    A = as.matrix(constraints(lp)$L),
    rhs = constraints(lp)$rhs,
    sense = constraints(lp)$dir,
    vtype = types(lp),
    lb = bounds$lb,
    ub = bounds$ub
  )
  soln <- gurobi::gurobi(model)
}


#' Solve Optimization Problem
#'
#' @param x list containing config and inputData
#' @export
AlteryxSolve <- function(x){
  class(x) <- c(class(x), paste0(x$config$inputMode, "_mode"))
  d2 <- constructModel(x)

  if (exists('returnSensitivity', x$config) && x$config$returnSensitivity) {
    # Use glpkAPI as the solver, if sensitivity analysis is required.
    class(d2$OP) <- c(class(d2$OP), 'glpkAPI')
    invisible(solveModel(d2$OP, solver = 'glpkAPI', d2$OPAttributes))
  } else {
    class(d2$OP) <- c(class(d2$OP), x$config$solver)
    invisible(solveModel(d2$OP, solver = x$config$solver))
  }
}
