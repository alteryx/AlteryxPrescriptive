context("fileInput-MPS")

config <- list(
  inputMode = "file",
  fileType = "MathProg",
  problemType = "lp",
  maximize = TRUE,
  solver = "glpk"
)


payload <- list(config = config, inputs = NULL)


test_that("Cell Tower LP is solved correctly by glpk", {
  payload$config$filePath = getSampleData("lpTwoVars.mod")
  payload$config$solver = 'glpk'
  sol = AlteryxSolve(payload)
  expect_equal(sol$objval, 34)
})


test_that("Cell Tower LP is solved correctly by gurobi", {
  skip_on_travis()
  library(gurobi)
  payload$config$filePath = getSampleData("lpTwoVars.mod")
  payload$config$solver = 'gurobi'
  out = capture.output(sol <- AlteryxSolve(payload))
  expect_equal(sol$objval, 34)
})
