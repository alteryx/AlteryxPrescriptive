context("matrixInput sensitivity")

## Mock up configuration and inputs  ----
config <- list(
  inputMode = "matrix",
  problemType = "lp",
  maximize = FALSE,
  returnSensitivity = TRUE
)

## Input data.frame with slam format ----
inputsSlam <- list()
inputsSlam$O <- data.frame(
  variable = c('x', 'y', 'z'),
  coefficient = c(1, 1, 2),
  lb = c(0, 0, 0),
  ub = c(Inf, Inf, Inf),
  type = c("C", "C", "C")
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

## Input data.frame with dense matrix format ----
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


payloadSlam  <- list(config = config, inputs = inputsSlam)
payloadDense <- list(config = config, inputs = inputsDense)

test_that("LP sensitivity, matrix mode (slam), with glpkAPI", {
  skip_if_not_installed('glpkAPI')
  payloadSlam$config$solver <- 'glpkAPI'
  df_sensitivity <- AlteryxSolve(payloadSlam)$sensitivity
  expect_equal(dim(df_sensitivity$constraintsRHS), c(4,10))
})


test_that("LP sensitivity, matrix mode (dense), with glpk", {
  skip_if_not_installed('glpkAPI')
  payloadDense$config$solver <- 'glpkAPI'
  df_sensitivity <- AlteryxSolve(payloadDense)$sensitivity
  expect_equal(dim(df_sensitivity$constraintsRHS), c(4,10))
})

