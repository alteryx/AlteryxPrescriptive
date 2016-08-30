context("matrix input, basic")

config <- list(
  inputMode = "matrix",
  problemType = "lp",
  solver = "glpk",
  maximize = TRUE,
  returnSensitivity = FALSE
)

inputs<- list()
inputs$O <- data.frame(
  variable = c('x1', 'x2'),
  coefficient = c(5, 4),
  lb = c(0, 0),
  ub = c(Inf, 2),
  type = c("C", "C")
)


inputs$A <- data.frame(
  constraint = c('c1', 'c2','c3'),
  x1 = c(6, 1, -1),
  x2 = c(4, 2, 1)
)

inputs$B <- data.frame(
  dir = c("<=", "<=", "<="),
  rhs = c(24, 6, 1)
)


payload <- list(config = config, inputs = inputs)

sol <- AlteryxSolve(payload)

test_that("Matrix Input, Basic LP", {
  expect_equal(sol$objval, 21)
})
