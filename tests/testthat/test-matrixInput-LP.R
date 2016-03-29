context("matrixInput")

## Mock up configuration and inputs  ----
config <- list(
  inputMode = "matrix",
  problemType = "lp",
  maximize = FALSE
)

## Input Tables Data-----
inputs <- list()
inputs$O <- data.frame(
  variable = c('x', 'y', 'z'),
  coefficient = c(1, 1, 2),
  lb = c(0, 0, 0),
  ub = c(Inf, Inf, Inf),
  type = c("B", "B", "B")
)

inputs$A <- data.frame(
  i = c(1, 1, 1, 2, 2),
  j = c(1, 2, 3, 1, 2),
  v = c(1, 2, 3, 1, 1)
)

inputs$B <- data.frame(
  dir = c("<=", ">="),
  rhs = c(4, 1)
)

# inputData$Q <- data.frame(
#   x1 = c(1, 2),
#   x2 = c(2, 7)
# )

payload <- list(config = config, inputs = inputs)

test_that("Solves matrix inputs correctly", {
  payload$config$solver <- 'glpk'
  sol <- AlteryxSolve(payload)
  expect_equal(sol$objval, 1)
})


test_that("Gurobi solves matrix inputs correctly", {
  skip_on_travis()
  skip_if_not_installed('gurobi')
  library(gurobi)
  payload$config$solver <- 'gurobi'
  res <- capture.output(sol <- AlteryxSolve(payload))
  expect_equal(sol$objval, 1)
})
