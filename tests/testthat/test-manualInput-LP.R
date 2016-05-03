context("manualInput-LP")

# Configuration
constraints = c(
  "3x1 + 4x2 + 2x3 <= 60",
  "2x1 + x2 + 2x3 <= 40",
  "x1 + 3x2 + 2x3 <= 80"
)
objective = (
  "2x1 + 4x2 + 3x3"
)
config <- list(
  constraints = constraints,
  objective = objective,
  maximize = TRUE,
  inputMode = 'manual',
  solver = 'glpk'
)

payload <- list(config = config, inputs = NULL)


test_that("Manual Input works for LP", {
  out = capture.output(sol <- AlteryxSolve(payload))
  expect_equal(sol$objval, 76.66667, tolerance = 0.001)
})


test_that("Manual Input works for LP", {
  payload$config$solver <- 'symphony'
  out = capture.output(sol <- AlteryxSolve(payload))
  expect_equal(sol$objval, 76.66667, tolerance = 0.001)
})


# Configuration with "*" and irregular white space
constraints1 = c(
  "3*x1 + 4*x2 + 2*x3 <= 60",
  "2*x1+x2 +2*x3 <= 40",
  "x1 +3*x2 + 2x3 <= 80"
)
objective1 = (
  "2* x1 +4*x2 + 3x3"
)
config1 <- list(
  constraints = constraints,
  objective = objective,
  maximize = TRUE,
  inputMode = 'manual',
  solver = 'glpk'
)

payload1 <- list(config = config1, inputs = NULL)


test_that("Manual Input works for LP", {
  out = capture.output(sol <- AlteryxSolve(payload1))
  expect_equal(sol$objval, 76.66667, tolerance = 0.001)
})
