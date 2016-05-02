context("fileInput, sensitivity")

# Configuration mockup
config <- list(
  inputMode = "file",
  fileType = "CPLEX_LP",
  filePath = getSampleData("table_chair.lp"),
  returnSensitivity = TRUE
)

payload <- list(config = config, inputs = NULL)

test_that("LP sensitivity, file mode (dense), with glpk", {
  payload$config$solver <- 'glpkAPI'
  sol <- AlteryxSolve(payload)
  df_sensitivity <- sol$sensitivity
  expect_equal(dim(df_sensitivity$constraintsRHS), c(4,10))
})

