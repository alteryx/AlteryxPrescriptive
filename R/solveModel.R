#' Solve model using Gurobi -----
#'
#' @keywords internal
ROI_solve_gurobi <- function(lp){
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

#' Solve model
#'
#' @param x x
#' @param solver solver
#' @import ROI ROI.plugin.glpk ROI.plugin.quadprog quadprog
solveModel <- function(x, solver = 'glpk'){
  if (solver == 'gurobi'){
    if (requireNamespace('gurobi')){
      library(gurobi)
      ROI_solve_gurobi(x)
    }
  } else {
    ROI::ROI_solve(x, solver = solver)
  }
}

#' Solve Optimization Problem
#'
#' @param x list containing config and inputData
#' @export
AlteryxSolve <- function(x){
  class(x) = c(class(x), paste0(x$config$inputMode, "_mode"))
  d2 <- constructModel(x)
  invisible(solveModel(d2, solver = x$config$solver))
}
