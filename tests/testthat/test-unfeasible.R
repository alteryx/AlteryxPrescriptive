context("Solutions not available")

config <- list(
  inputMode = "matrix",
  problemType = "lp",
  maximize = TRUE,
  returnSensitivity = FALSE
)

inputs <- list()
inputs$O <- data.frame(
  variable = c('A1', 'A2', 'A3', 'B1', 'B2', 'C1',
               'C2', 'C3', 'D1', 'D2', 'D3', 'D4'),
  coefficient = c(3.7, 5.2, 6.1, 9.3, 9.6, 4.8, 7.2, 9.1, 2.6, 5.4, 5.8, 6.9),
  lb = rep(0, 12),
  ub = rep(1, 12),
  type = rep('B', 12)
)

inputs$A <- data.frame(
  A1 = c(12, 1, 0, 0, 0),
  A2 = c(16, 1, 0, 0, 0),
  A3 = c(18, 1, 0, 0, 0),
  B1 = c(21, 0, 1, 0, 0),
  B2 = c(24, 0, 1, 0, 0),
  C1 = c(18, 0, 0, 1, 0),
  C2 = c(21, 0, 0, 1, 0),
  C3 = c(28, 0, 0, 1, 0),
  D1 = c(14, 0, 0, 0, 1),
  D2 = c(15, 0, 0, 0, 1),
  D3 = c(18, 0, 0, 0, 1),
  D4 = c(24, 0, 0, 0, 1)
)


inputs$B <- data.frame(
  dir = c("<=", "==", "==", "==", "=="),
  rhs = c(60, 1, 1, 1, 1)
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
