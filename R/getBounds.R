getBounds_matrix <- function(idata){
  li <- 1:NROW(idata$O)
  ui <- 1:NROW(idata$O)
  lb <- ifelse(idata$O$lb, idata$O$lb, 0)
  ub <- ifelse(idata$O$ub, idata$O$ub, Inf)
  nobj <- max(li, ui)
  bounds <- V_bound(li = li, ui = ui, lb = lb, ub = ub, nobj = nobj)
  # Need to treat bounds very carefully. V_bound strips default bounds which are
  # 0 for lower and Inf for upper. As a result it will return a list, with zero
  # bounds. But ROI_solve requires bounds to be NULL if it is expected to use
  # defaults.
  if ((length(bounds$lower$ind) == 0) && (length(bounds$upper$ind) == 0)){
    bounds <- NULL
  }
  return(bounds)
}

# Get Bounds from Object -----
getBounds <- function(mod){
  li <- mod$bounds$lower$ind
  ui <- mod$bounds$upper$ind
  lb <- mod$bounds$lower$val
  ub <- mod$bounds$upper$val
  # we force evaluation of nobj due to lazy evaluation error in V_bound
  nobj <- max(li, ui)
  bounds <- V_bound(li = li, ui = ui, lb = lb, ub = ub, nobj = nobj)
  # Need to treat bounds very carefully. V_bound strips default bounds which are
  # 0 for lower and Inf for upper. As a result it will return a list, with zero
  # bounds. But ROI_solve requires bounds to be NULL if it is expected to use
  # defaults.
  if ((length(bounds$lower$ind) == 0) && (length(bounds$upper$ind) == 0)){
    bounds <- NULL
  }
  return(bounds)
}

# Get bounds for Gurobi
getBounds_gurobi <- function(mod){
  li <- mod$bounds$lower$ind
  ui <- mod$bounds$upper$ind
  lb <- mod$bounds$lower$val
  ub <- mod$bounds$upper$val
  # we force evaluation of nobj due to lazy evaluation error in V_bound
  if (is.null(li) && is.null(ui)){
    return(list(lb = NULL, ub = NULL))
  } else {
    nobj <- max(li, ui)
    lb_ <- rep(0, nobj)
    if (!is.null(li)){
      lb_[li] <- lb
    }

    ub_ <- rep(Inf, nobj)
    if (!is.null(ui)){
      ub_[ui] <- ub
    }
    list(lb = lb_, ub = ub_)
  }
}
