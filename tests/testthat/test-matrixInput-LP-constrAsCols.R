## Mock up configuration and inputs  ----
config <- list(
  inputMode = "matrix",
  problemType = "lp",
  maximize = FALSE,
  solver = 'glpk',
  returnSensitivity = FALSE
)

inputs <- list()

inputs$O <- data.frame(
  variable = c('x', 'y', 'z'),
  coefficient = c(1, 1, 2),
  lb = c(0, 0, 0),
  ub = c('Inf', 'Inf', 'Inf'),
  type = c("B", "B", "B")
)

inputs$A <- data.frame(
  variable = c('x', 'yy', 'z'),
  c1 = c(1, 2, 3),
  c2 = c(1, 1, 0)
)

inputs$B <- data.frame(
  dir = c("<=", ">="),
  rhs = c(4, 1)
)



# Assemble payload
# payload  <- list(config = config, inputs = inputs)
# sol <- AlteryxSolve(payload)
#
# x <- payload
# class(x) <- c(class(x), paste0(x$config$inputMode, "_mode"))
# idata <- AlteryxPrescriptive:::processData(x$inputs)

