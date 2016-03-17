## Question Inputs ----
config <- list(
  inputMode = "matrix",
  problemType = "qp",
  maximize = FALSE,
  solver = "quadprog"
)

inputs <- list()
inputs$O <- data.frame(
  variable = c('x', 'y', 'z'),
  coefficient = c(2, 0, 0),
  lb = c(0, 0, 0),
  ub = c(Inf, Inf, Inf),
  type = c('C', 'C', 'C')
)

inputs$A <- data.frame(
  i = c(1, 1, 1, 2, 2),
  j = c(1, 2, 3, 1, 2),
  v = c(1, 2, 3, 1, 1)
)

inputs$B <- data.frame(
  dir = c(">=", ">="),
  rhs  = c(4, 1)
)

inputs$Q <- data.frame(
  i = c(1, 1, 2, 2, 2, 3, 3),
  j = c(1, 2, 1, 2, 3, 2, 3),
  v = c(2, 1, 1, 2, 1, 1, 2)
)

payload = list(config = config, inputs = inputs)

test_that("Matrix Input Mode, Quadratic Programming", {
  sol <- AlteryxSolve(payload)
  expect_that(abs(sol$objval-2.1111), is_less_than(0.0001))
})
