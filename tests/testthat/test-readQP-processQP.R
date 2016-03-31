context("Read a model file and return the parsed Q components")

expected <- data.frame(
  x1 = c("a", "b", "a", "b"),
  x2 = c("a", "a", "b", "b"),
  x3 = c("a^2", "b*a", "a*b", "b^2"),
  values = c("1", NA, "+4", "+7"),
  stringsAsFactors = FALSE
)

# utility for generating a vector of different model string variations
generateModelVariations <- function(model, target, options) {
  sapply(options, function(option) {
    str_replace(model, target, option)
  })
}

test_that("processQP can read model a file and return the Q components", {
  test_path <- getSampleData("simple_qp", TRUE)
  modelFile <- readFileContents(test_path)
  list[model, vecMap] <- processQP(modelFile, "CPLEX_LP")
  expect_is(model, c("MP_data_from_file", "MILP"))
  expect_is(vecMap, "data.frame")
  expect_equal(length(vecMap), 4)
  expect_equal(names(vecMap), c("x1", "x2", "x3", "values"))
  for (i in names(vecMap)) {
    expect_equal(vecMap$i, expected$i)
  }
  expect_false(all(is.na(vecMap)))
})


test_that("processQP handles all constraint variations correctly", {
  options <- c("such that", "ST.", "s.t.", "sT", "sUbJecT tO")

  test_path <- getSampleData("simple_qp", TRUE)
  modelFile <- readFileContents(test_path)
  testCases <- generateModelVariations(modelFile, "Subject To", options)

  for (case in testCases) {
    list[model, vecMap] <- processQP(case, "CPLEX_LP")
    expect_is(model, c("MP_data_from_file", "MILP"))
    expect_is(vecMap, "data.frame")
    expect_equal(length(vecMap), 4)
    expect_equal(names(vecMap), c("x1", "x2", "x3", "values"))
    for (i in names(vecMap)) {
      expect_equal(vecMap$i, expected$i)
    }
    expect_false(all(is.na(vecMap)))
  }
})

test_that("processQP handles max/min variations", {
  options <- c("min", "max", "Maximize", "minimize", "mAxIMUm", "MInIMUm")

  test_path <- getSampleData("simple_qp", TRUE)
  modelFile <- readFileContents(test_path)
  testCases <- generateModelVariations(modelFile, "Minimize", options)

  for (case in testCases) {
    list[model, vecMap] <- processQP(case, "CPLEX_LP")
    expect_is(model, c("MP_data_from_file", "MILP"))
    expect_is(vecMap, "data.frame")
    expect_equal(length(vecMap), 4)
    expect_equal(names(vecMap), c("x1", "x2", "x3", "values"))
    for (i in names(vecMap)) {
      expect_equal(vecMap$i, expected$i)
    }
    expect_false(all(is.na(vecMap)))
  }
})
