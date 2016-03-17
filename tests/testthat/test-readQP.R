context("read CPLEX QP files")

test_that("readCPLEXFile can read a CPLEX file successfully", {
  test_path <- getSampleData("simple_qp", TRUE)
  file <- readCPLEXFile(test_path)
  expect_equal(file, "Minimize\nobj: a + b + [ a^2 + 4 a * b + 7 b^2 ]/2\nSubject To\nc1: a + b >= 10\nEnd\n")
})

test_that("readCPLEXFile errors when the user doesn't point to a CPLEX file", {
  test_path <- getSampleData("non_CPLEX.txt", TRUE)
  expect_error(readCPLEXFile(test_path), "You must use a CPLEX LP model file \\(ending in '.lp'\\)")
})

test_that("readModelFile correctly short-circuits when it encounters a model without a quadratic piece", {
  test_path <- getSampleData("lp_example", TRUE)
  expect_message(result <- readModelFile(test_path), "^No quadratic elements detected\\. .+$")
  # result <- suppressMessages(readQP(test_path))
  expect_is(result, c("MP_data_from_file", "MILP"))
})

test_that("readModelFile can correctly turn a QP model file into a model object", {
  test_path <- getSampleData("qp_3_terms", TRUE)
  model <- readModelFile(test_path)
  expect_is(model, c("MP_data_from_file", "MILP"))
  expect_equal(names(model$objective), c("c", "Q"))
  list[c, Q] <- model$objective
  expect_equal(c$v, c(1, 1, 2))
  expect_is(c, "simple_triplet_matrix")
  expect_equal(Q$v, c(1.0, 2.0, 2.5, 2.0, 7.0, -1.0, 2.5, -1.0, 3.0))
  expect_is(Q, "simple_triplet_matrix")
})

test_that("readQP can handle in an insane LP file and return the correct object", {
  test_path <- getSampleData("alldiet", TRUE)
  expect_message(model <- readModelFile(test_path), "No quadratic elements detected. Defaulting to Rglpk_read_file.")
  expect_output(model, "A linear program with 15 objective variables")
  expect_equal(attr(model, "n_objective_vars"), 15)
  expect_equal(attr(model, "n_objective_vars"), 15)
  expect_equal(attr(model, "n_constraints"), 8)
  expect_equal(attr(model, "objective_name"), "DOLLARS")
})
