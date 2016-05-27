context("No feasible solution scenario")

config <- list(
  inputMode = "matrix",
  problemType = "lp",
  maximize = FALSE,
  returnSensitivity = FALSE
)

inputs <- list()
inputs$O <- data.frame(
  variable = c('x','y'),
  coefficient = c(1, 1),
  lb = c(6, 6),
  ub = c(Inf, Inf),
  type = rep('C', 2)
)

inputs$A <- data.frame(
  x = 1,
  y = 1
)

inputs$B <- data.frame(
  dir = "<=",
  rhs = 11
)

payload <- list(config = config, inputs = inputs)

test_that("No feasible solution, with glpk", {
  payload$config$solver <- 'glpk'
  sol <- AlteryxSolve(payload)
  expect_equal(sol$status$code, 1)
})


test_that("No feasible solution, with symphony", {
  skip_if_not_installed('Rsymphony')
  payload$config$solver <- 'symphony'
  sol <- AlteryxSolve(payload)
  expect_equal(sol$status$code, 1)
})




