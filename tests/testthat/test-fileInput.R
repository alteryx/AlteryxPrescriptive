context("File inputs work correctly")

## Mock up configuration and inputs  ----
config <- list(
  inputMode = "file",
  fileType = "CPLEX_LP",
  problemType = "lp",
  maximize = TRUE,
  solver = "glpk"
)


payload <- list(config = config, inputs = NULL)


test_that("Cell Tower LP is solved correctly by glpk", {
  payload$config$filePath = getSampleData("cell_tower.lp")
  payload$config$solver = 'glpk'
  sol = AlteryxSolve(payload)
  expect_equal(sol$objval, 7051)
})


test_that("Cell Tower LP is solved correctly by gurobi", {
  skip_on_travis()
  skip_if_not_installed('gurobi')
  library(gurobi)
  payload$config$filePath = getSampleData("cell_tower.lp")
  payload$config$solver = 'gurobi'
  out = capture.output(sol <- AlteryxSolve(payload))
  expect_equal(sol$objval, 7051)
})


