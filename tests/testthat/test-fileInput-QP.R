context("fileInput-QP")

## Mock up configuration and inputs  ----
config <- list(
  inputMode = "file",
  fileType = "CPLEX_LP",
  problemType = "qp",
  maximize = TRUE,
  solver = "quadprog"
)

#payload = append(config, list(idata = NULL))
payload = list(config = config, inputs = NULL)

test_that('Quadprog solves Simple QP', {
  payload$config$filePath = getSampleData('simple_qp.lp')
  sol <- AlteryxSolve(payload)
  expect_equal(sol$objval, 60)
})


test_that('Gurobi solves Simple QP', {
  skip_on_travis()
  # I have no idea why this line is required while testing.
  library(gurobi)
  payload$config$filePath = getSampleData('simple_qp.lp')
  payload$config$solver = 'gurobi'
  res <- capture.output(sol <- AlteryxSolve(payload))
  expect_equal(sol$objval, 60)
})

test_that('Gurobi solves Complex QP', {
  skip_on_travis()
  library(gurobi)
  payload$config$filePath = getSampleData('qp_complex.lp')
  payload$config$solver = 'gurobi'
  res <- capture.output(sol <- AlteryxSolve(payload))
  expect_equal(sol$objval, 3)
})


# test_that('Quadprog errors solving Comples QP', {
#   payload$config$filePath = getSampleData('simple_qp.lp')
#   payload$config$solver = 'quadprog'
#   expect_error(AlteryxSolve(payload))
# })


# test_that('Quadprog solves QP with three terms', {
#   payload$filePath = getSampleData('qp_3_terms.lp')
#   payload$solver = 'quadprog'
#   sol <- AlteryxSolve(payload)
#   expect_equal(is.na(sol$objval), TRUE)
# })
#
#
# test_that('Quadprog solves Simple QP', {
#   payload$filePath = getSampleData('qp_3_terms.lp')
#   payload$solver = 'gurobi'
#   res <- capture.output(sol <- AlteryxSolve(payload))
#   expect_equal(is.na(sol$objval), TRUE)
# })
