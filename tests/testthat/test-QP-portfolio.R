context("Quadratic Programming: portfolio")

config <- list(
  inputMode = "matrix",
  problemType = "qp",
  maximize = FALSE,
  solver = 'quadprog'
)

input <- list()
input$O <- data.frame(
  variable = c('LNKD', 'DATA', 'STX', 'AAPL', 'GOOG'),
  coefficient = c(0.000648, 0.000879, 0.00187, 0.00038, -0.000521),
  lb = rep(0, 5),
  ub = rep(Inf, 5),
  type = rep('C', 5)
)

input$A <- data.frame(
  LNKD = 1,
  DATA = 1,
  STX = 1,
  APPL = 1,
  GOOG = 1
)

input$B <- data.frame(
  dir = "==",
  rhs = 1
)

input$Q <- data.frame(
  LNKD = c(0.00178502953161508,	0.00153416322507441,	0.000100828905329932,	0.000212531859307057, 0.000268932719505676),
  DATA = c(0.00153416322507441,	0.00270550482931895,	0.000149316367435453, 0.000267908021216039,	0.000253165846767602),
  STX = c(0.000100828905329932,	0.000149316367435453,	0.00147954479859754,	0.000185229071429616,	7.11285187509678e-05),
  APPL = c(0.000212531859307057,0.000267908021216039,	0.000185229071429616,	0.0003011892221733,	0.000120543449816519),
  GOOG = c(0.000268932719505676,0.000253165846767602,	7.11285187509678e-05,	0.000120543449816519,	0.00024638404509849)
)



test_that("Field Mapping works correctly", {
  library(ROI)
  payload <- list(config = config, inputs = input)
  sol <- AlteryxSolve(payload)
  expect_equal(sol$objval, -0.000397808)
})
