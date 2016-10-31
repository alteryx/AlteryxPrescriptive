context('Field Mapping works correctly')

test_that("Variables in rows with OAB inputs", {
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
  expect_equal(sol$objval, 21)
})

test_that("Constraint in rows with OAB inputs", {
  config <- list(
    inputMode = "matrix",
    problemType = "lp",
    solver = "glpk",
    displayFieldMapO = FALSE,
    maximize = TRUE,
    returnSensitivity = FALSE
  )

  inputs<- list()
  inputs$O <- data.frame(
    variable = c('P1', 'P2', 'P3', 'J1', 'J2', 'M1', 'M2', 'M3',
            'B1', 'B2', 'B3', 'B4'),
    coefficient = c(3.7, 5.2, 6.1, 9.3, 9.6, 4.8, 7.2, 9.1, 2.6, 5.4, 5.8, 6.9),
    lb = rep(0, 12),
    ub = rep(1, 12),
    type = rep("B", 12)
  )


  inputs$A <- data.frame(
    #constraint = sapply(seq(1,5), function(x) paste0('c', x)),
    P1 = c(12, 1, 0, 0, 0),
    P2 = c(16, 1, 0, 0, 0),
    P3 = c(18, 1, 0, 0, 0),
    J1 = c(21, 0, 1, 0, 0),
    J2 = c(24, 0, 1, 0, 0),
    M1 = c(18, 0, 0, 1, 0),
    M2 = c(21, 0, 0, 1, 0),
    M3 = c(28, 0, 0, 1, 0),
    B1 = c(1, 0, 0, 0, 1),
    B2 = c(15, 0, 0, 0, 1),
    B3 = c(18, 0, 0, 0, 1),
    B4 = c(24, 0, 0, 0, 1)
  )

  inputs$B <- data.frame(
    dir = rep("<=", 5),
    rhs = c(80, 1, 1, 1, 1)
  )


  payload <- list(config = config, inputs = inputs)

  sol <- AlteryxSolve(payload)
  expect_equal(sol$objval, 21)
})
