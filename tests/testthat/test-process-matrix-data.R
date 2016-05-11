context("processData-LP")

config <- list(
  inputMode = "matrix",
  maximize = TRUE,
  problemType = "lp",
  solver = 'glpk',
  returnSensitivity = FALSE
)

readInputsFromXL <- function(path){
  library(readxl)
  sheets <- excel_sheets(path)
  inputs <- setNames(lapply(sheets, read_excel, path = path), sheets)
}

library(testthat)
testFactory <- function(paths, solver = 'glpk', config){
  config$solver <- solver
  results <- lapply(paths, function(path){
    test_that(paste('Testing ', path), {
      x <- list(config = config, inputs = readInputsFromXL(path))
      out <- AlteryxSolve(x)
      expect_equal(out$objval, 25000)
    })
  })
}

f <- list.files(
  system.file('extdata', package = 'AlteryxPrescriptive'),
  pattern = '.xlsx',
  full.names = TRUE
)
testFactory(f, solver = 'glpk', config)
