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
testFactory <- function(paths, solver = 'glpk', config, sol){
  config$solver <- solver
  results <- lapply(paths, function(path){
    test_that(paste('Testing ', path), {
      skip_if_not_installed('readxl')
      if (grepl("min", basename(path))){
        config$maximize = FALSE
      }
      x <- list(config = config, inputs = readInputsFromXL(path))
      x$inputs <- lapply(x$inputs, as.data.frame)
      out <- AlteryxSolve(x)
      return(TRUE)
      expect_equal(out$objval, sol[[path]])
    })
  })
}

f <- list.files(
  system.file('extdata', package = 'AlteryxPrescriptive'),
  pattern = '.xlsx',
  full.names = TRUE
)
sol <- setNames(c(rep(25000, 3), 25000,3200), f)
testFactory(f, solver = 'glpk', config, sol)
