context("parseQ can handle various permutations of Q equations")

test_that("parse a basic 2 term QP", {
  testQ <- "a^2+4a*b+7b^2"
  varNames <- c("a", "b")
  tokens <- tokenizeQ(testQ)
  expect_equal(tokens[,3], c("a^2", "a*b", "b^2"))
  expect_equal(tokens[,2], c("", "+4", "+7"))
  testCoefs <- getCoefNames(tokens)
  expect_equal(testCoefs, c("a^2" = "+1", "a*b" = "+4", "b^2" = "+7"))
  createVarPairs(varNames, testCoefs)
})

test_that("parse a 2 term QP with non-integer coefs", {
  testQ <- "a^2+4.867a*b-.012b^2"
  varNames <- c("a", "b")
  tokens <- tokenizeQ(testQ)
  expect_equal(tokens[,3], c("a^2", "a*b", "b^2"))
  expect_equal(tokens[,2], c("", "+4.867", "-.012"))
  testCoefs <- getCoefNames(tokens)
  expect_equal(testCoefs, c("a^2" = "+1", "a*b" = "+4.867", "b^2" = "-.012"))
  createVarPairs(varNames, testCoefs)
})

test_that("parse var names that end with numbers", {
  testQ <- "a4^2+4a4*b6+7b6^2"
  varNames <- c("a4", "b6")
  tokens <- tokenizeQ(testQ)
  expect_equal(tokens[,3], c("a4^2", "a4*b6", "b6^2"))
  expect_equal(tokens[,2], c("", "+4", "+7"))
  testCoefs <- getCoefNames(tokens)
  expect_equal(testCoefs, c("a4^2" = "+1", "a4*b6" = "+4", "b6^2" = "+7"))
  createVarPairs(varNames, testCoefs)
})

test_that("parse a 2 term QP with complicated var names", {
  testQ <- "d!4g^2+7f#a#inf*d!4g+f#a#inf^2"
  varNames <- c("d!4g", "f#a#inf")
  tokens <- tokenizeQ(testQ)
  expect_equal(tokens[,3], c("d!4g^2", "f#a#inf*d!4g", "f#a#inf^2"))
  expect_equal(tokens[,2], c("", "+7", "+"))
  testCoefs <- getCoefNames(tokens)
  expect_equal(testCoefs, c("d!4g^2" = "+1", "f#a#inf*d!4g" = "+7", "f#a#inf^2" = "+1"))
  createVarPairs(varNames, testCoefs)
})

test_that("parse a Q equation with missing coefficients and a leading negative", {
  testQ <- "-a^2-a*b+7b^2"
  varNames <- c("a", "b")
  tokens <- tokenizeQ(testQ)
  expect_equal(nrow(tokens), 3)
  expect_equal(tokens[,3], c("a^2", "a*b", "b^2"))
  expect_equal(tokens[,2], c("-", "-", "+7"))
  testCoefs <- getCoefNames(tokens)
  expect_equal(testCoefs, c("a^2" = "-1", "a*b" = "-1", "b^2" = "+7"))
})

test_that("parse a 3-term QP with all permutations present", {
  testQ <- "a^2+4a*b+7b^2+3c^2+5a*c-2c*b"
  varNames <- c("a", "b", "c")
  tokens <- tokenizeQ(testQ)
  expect_equal(tokens[,3], c("a^2", "a*b", "b^2", "c^2", "a*c", "c*b"))
  expect_equal(tokens[,2], c("", "+4", "+7", "+3", "+5", "-2"))
  testCoefs <- getCoefNames(tokens)
  expect_equal(testCoefs, c("a^2" = "+1", "a*b" = "+4", "b^2" = "+7", "c^2" = "+3", "a*c" = "+5", "c*b" = "-2"))
})

test_that("parse a 3-term QP with some permutations missing", {
  testQ <- "a^2+7b^2+3c^2+5a*c-2c*b"
  varNames <- c("a", "b", "c")
  tokens <- tokenizeQ(testQ)
  expect_equal(tokens[,3], c("a^2", "b^2", "c^2", "a*c", "c*b"))
  expect_equal(tokens[,2], c("", "+7", "+3", "+5", "-2"))
  testCoefs <- getCoefNames(tokens)
  expect_equal(testCoefs, c("a^2" = "+1", "b^2" = "+7", "c^2" = "+3", "a*c" = "+5", "c*b" = "-2"))
})

test_that("parse a 3-term QP with missing permutations, a leading negative, and blank coefs", {
  testQ <- "-a^2+7b^2-c^2+a*c-2c*b"
  varNames <- c("a", "b", "c")
  tokens <- tokenizeQ(testQ)
  expect_equal(tokens[,3], c("a^2", "b^2", "c^2", "a*c", "c*b"))
  expect_equal(tokens[,2], c("-", "+7", "-", "+", "-2"))
  testCoefs <- getCoefNames(tokens)
  expect_equal(testCoefs, c("a^2" = "-1", "b^2" = "+7", "c^2" = "-1", "a*c" = "+1", "c*b" = "-2"))
})
