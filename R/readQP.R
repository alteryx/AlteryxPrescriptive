#' Convert model files to ROI-compatible model objects
#'
#' Read and parse a model file (including quadratic objectives).
#' @param filePath The path to the model file
#' @param type File type. Defaults to "CPLEX_LP"
#' @return An ROI-compatible model object
#' @export
readModelFile <- function(filePath, type = "CPLEX_LP") {
  file <- readFileContents(filePath)
  if (!isQP(file) || type != "CPLEX_LP") {
    message("No quadratic elements detected. Defaulting to Rglpk_read_file.\n")
    return(Rglpk_read_file(filePath, type = type))
  }
  # R CMD CHECK complains about this!!!!
  # fyi, this is apparently a common issue: http://stackoverflow.com/a/23476834/5870180
  # list[model, vecMap] <- processQP(file, type)
  res <- processQP(file, type)
  qMat <- createQMatrix(res$vecMap)
  return(createQPModel(res$model, qMat))
}

# Insert the Q matrix into the model object's `objective` element. model$objective will now have
# two child elements: c (the original/linear objective) and Q (the quadratic element)
createQPModel <- function(model, qMat) {
  c <- model$objective
  model$objective <- list(c = c, Q = qMat)
  return(model)
}

readFileContents <- function(filePath) {
  if (!isSupportedFormat(filePath)) {
    stop(
      paste(
        "You must use one of the following file formats: ",
        paste("\t", getValidFormats(), collapse="\n"),
        sep = "\n"
      )
    )
  }
  file <- readChar(normalizePath(filePath), file.info(filePath)$size)
  return(file)
}
