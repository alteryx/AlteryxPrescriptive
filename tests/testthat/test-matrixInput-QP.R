context("Quadratic Programming")

## Question Inputs ----
config <- list(
  inputMode = "matrix",
  problemType = "qp",
  maximize = FALSE
)

## Input data.frame with slam format ----
inputsSlam <- list()
inputsSlam$O <- data.frame(
  variable = c('x', 'y', 'z'),
  coefficient = c(2, 0, 0),
  lb = c(0, 0, 0),
  ub = c(Inf, Inf, Inf),
  type = c('C', 'C', 'C')
)

inputsSlam$A <- data.frame(
  i = c(1, 1, 1, 2, 2),
  j = c(1, 2, 3, 1, 2),
  v = c(1, 2, 3, 1, 1)
)

inputsSlam$B <- data.frame(
  dir = c(">=", ">="),
  rhs  = c(4, 1)
)

inputsSlam$Q <- data.frame(
  i = c(1, 1, 2, 2, 2, 3, 3),
  j = c(1, 2, 1, 2, 3, 2, 3),
  v = c(2, 1, 1, 2, 1, 1, 2)
)

## Input data.frame with dense matrix format ----
inputsDense <- list()
inputsDense$O <- inputsSlam$O
inputsDense$A <- data.frame(
  x1 = c(1,1),
  x2 = c(2,1),
  x3 = c(3,0)
)
inputsDense$B <- inputsSlam$B
inputsDense$Q <- data.frame(
  x1 = c(2,1,0),
  x2 = c(1,2,1),
  x3 = c(0,1,2)
)

## Create payload with different input matrix format ----
payloadSlam  <- list(config = config, inputs = inputsSlam)
payloadDense <- list(config = config, inputs = inputsDense)


## Tests
test_that("quadratic programming, matrix mode (slam), with quadprog", {
  payloadSlam$config$solver <- 'quadprog'
  sol <- AlteryxSolve(payloadSlam)
  expect_that(abs(sol$objval-2.1111), is_less_than(0.0001))
})


test_that("quadratic programming, matrix mode (dense), with quadprog", {
  payloadDense$config$solver <- 'quadprog'
  sol <- AlteryxSolve(payloadDense)
  expect_that(abs(sol$objval-2.1111), is_less_than(0.0001))
})


test_that("quadratic programming, matrix mode (slam), with quadprog", {
  skip_on_travis()
  library(gurobi)
  payloadSlam$config$solver <- 'gurobi'
  res <- capture.output(sol <- AlteryxSolve(payloadSlam))
  expect_that(abs(sol$objval-2.1111), is_less_than(0.0001))
})


test_that("quadratic programming, matrix mode (dense), with quadprog", {
  skip_on_travis()
  library(gurobi)
  payloadDense$config$solver <- 'gurobi'
  res <- capture.output(sol <- AlteryxSolve(payloadDense))
  expect_that(abs(sol$objval-2.1111), is_less_than(0.0001))
})
