config <- list(
  inputMode = "matrix",
  problemType = "lp",
  solver = "glpk",
  displayFieldMapO = TRUE,
  constraintMode = "conInRow",
  nameVar = "var",
  nameCoef = "coeff",
  nameLower = "l",
  nameUpper = "u",
  nameType = "t",
  maximize = TRUE,
  returnSensitivity = FALSE
)

inputs<- list()
inputs$O <- data.frame(
  var = c('x1', 'x2'),
  coeff = c(5, 4),
  l = c(0, 0),
  u = c(Inf, 2),
  t = c("C", "C")
)


inputs$A <- data.frame(
  cons = c('c1', 'c2','c3'),
  x1 = c(6, 1, -1),
  x2 = c(4, 2, 1)
)

inputs$B <- data.frame(
  dir = c("<=", "<=", "<="),
  rhs = c(24, 6, 1)
)


payload <- list(config = config, inputs = inputs)

sol <- AlteryxSolve(payload)
