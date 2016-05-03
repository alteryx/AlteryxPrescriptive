context("File inputs work correctly")

## Mock up configuration and inputs  ----
config <- list(
  inputMode = "file",
  fileType = "CPLEX_LP",
  filePath = getSampleData("cell_tower.lp"),
  maximize = TRUE,
  solver = "glpk"
)


payload <- list(config = config, inputs = NULL)
row_optimals <- c(1.0,0.0,1.0,0.0,0.0,0.0,1.0,0.0,0.0,14.2)

test_that("Cell Tower LP is solved correctly by glpk", {
  payload$config$solver = 'glpk'
  sol = AlteryxSolve(payload)
  expect_equal(sol$objval, 7051)
  expect_equal(sol$row_activity$optimals, row_optimals)
})


test_that("Cell Tower LP is solved correctly by gurobi", {
  skip_on_travis()
  skip_if_not_installed('gurobi')
  library(gurobi)
  payload$config$solver = 'gurobi'
  out = capture.output(sol <- AlteryxSolve(payload))
  expect_equal(sol$objval, 7051)
  expect_equal(sol$row_activity$optimals, row_optimals)
})


test_that("Cell Tower LP is solved correctly by symphony", {
  payload$config$solver = 'symphony'
  sol = AlteryxSolve(payload)
  expect_equal(sol$objval, 7051)
  expect_equal(sol$row_activity$optimals, row_optimals)
})
