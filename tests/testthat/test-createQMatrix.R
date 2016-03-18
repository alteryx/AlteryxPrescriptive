context("Create Q matrix from a coefficient mapping")

test_that("createQMatrix works with a basic example", {
  expected <- as.simple_triplet_matrix(matrix(c(1, 2, 2, 7), nrow = 2, ncol = 2))
  path <- getSampleData("simple_qp", TRUE)
  file <- readFileContents(path)
  list[,qVec] <- processQP(file, type = "CPLEX_LP")
  testQ <- createQMatrix(qVec)
  expect_equal(testQ, expected)
})

test_that("createQMatrix works with >2 terms", {
  expected <- as.simple_triplet_matrix(
    matrix(
      c(1, 2, 2.5, 2, 7, -1, 2.5, -1, 3),
      nrow = 3,
      ncol = 3
      )
    )
  path <- getSampleData("qp_3_terms.lp", TRUE)
  file <- readFileContents(path)
  list[,qVec] <- processQP(file, type = "CPLEX_LP")
  testQ <- createQMatrix(qVec)
  expect_equal(testQ, expected)
})

test_that("createQMatrix works with missing terms", {
  expected <- as.simple_triplet_matrix(
    matrix(
      c(1, 0, 2.5, 0, 7, -1, 2.5, -1, 3),
      nrow = 3,
      ncol = 3
      )
    )
  path <- getSampleData("qp_missing_term", TRUE)
  file <- readFileContents(path)
  list[,qVec] <- processQP(file, type = "CPLEX_LP")
  testQ <- createQMatrix(qVec)
  expect_equal(testQ, expected)
})

test_that("createQMatrix errors when receiving incorrect input", {
  path <- getSampleData("qp_3_terms", TRUE)
  file <- readFileContents(path)
  list[,qVec] <- processQP(file, type = "CPLEX_LP")
  testQ <- createQMatrix(qVec)
  expect_error(createQMatrix(subset(qVec, !is.na(qVec$values))),
               "The vector you provided is not the correct size")
})
