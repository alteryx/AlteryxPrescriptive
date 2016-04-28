context("matrixInput")

## Mock up configuration and inputs  ----
config <- list(
  inputMode = "matrix",
  problemType = "lp",
  maximize = FALSE,
  returnSensitivity = FALSE
)

## Case1: Input data.frame with slam format ----
inputsSlam <- list()
inputsSlam$O <- data.frame(
  variable = c('x', 'y', 'z'),
  coefficient = c(1, 1, 2),
  lb = c(0, 0, 0),
  ub = c(Inf, Inf, Inf),
  type = c("B", "B", "B")
)


# [1, 2, 3]
# [1, 1, 0]
inputsSlam$A <- data.frame(
  i = c(1, 1, 1, 2, 2),
  j = c(1, 2, 3, 1, 2),
  v = c(1, 2, 3, 1, 1)
)

inputsSlam$B <- data.frame(
  dir = c("<=", ">="),
  rhs = c(4, 1)
)

## Case2: Input data.frame with dense matrix format ----
inputsDense <- list()
inputsDense$O <- inputsSlam$O

# [1, 2, 3]
# [1, 1, 0]
inputsDense$A <- data.frame(
  x1 = c(1, 1),
  x2 = c(2, 1),
  x3 = c(3, 0)
)

inputsDense$B <- inputsSlam$B


## Case3: Input data.frame with dense matrix format without "B" input ----
inputsDense1 <- list()
inputsDense1$O <- inputsSlam$O
inputsDense1$A <- data.frame(
  x1 = c(1, 1),
  x2 = c(2, 1),
  x3 = c(3, 0),
  dir = c("<=", ">="),
  rhs = c(4, 1)
)


# Assemble payload
payloadSlam  <- list(config = config, inputs = inputsSlam)
payloadDense <- list(config = config, inputs = inputsDense)
payloadDense1 <- list(config = config, inputs = inputsDense1)


# Tests
test_that("linear programming, matrix mode (slam), with glpk", {
  payloadSlam$config$solver <- 'glpk'
  sol <- AlteryxSolve(payloadSlam)
  expect_equal(sol$objval, 1)
})


test_that("linear programming, matrix mode (dense), with glpk", {
  payloadDense$config$solver <- 'glpk'
  sol <- AlteryxSolve(payloadDense)
  expect_equal(sol$objval, 1)
})

test_that("linear programming, matrix mode (dense, no B input), with glpk", {
  payloadDense1$config$solver <- 'glpk'
  sol <- AlteryxSolve(payloadDense1)
  expect_equal(sol$objval, 1)
})


test_that("linear programming, matrix mode (slam), with gurobi", {
  skip_on_travis()
  skip_if_not_installed('gurobi')
  library(gurobi)
  payloadSlam$config$solver <- 'gurobi'
  res <- capture.output(sol <- AlteryxSolve(payloadSlam))
  expect_equal(sol$objval, 1)
})

test_that("linear programming, matrix mode (dense), with gurobi", {
  skip_on_travis()
  skip_if_not_installed('gurobi')
  library(gurobi)
  payloadDense$config$solver <- 'gurobi'
  res <- capture.output(sol <- AlteryxSolve(payloadDense))
  expect_equal(sol$objval, 1)
})


test_that("linear programming, matrix mode (dense, no B input), with gurobi", {
  skip_on_travis()
  skip_if_not_installed('gurobi')
  library(gurobi)
  payloadDense1$config$solver <- 'gurobi'
  res <- capture.output(sol <- AlteryxSolve(payloadDense1))
  expect_equal(sol$objval, 1)
})
