#' @importFrom stringr str_detect
#' @keywords internal
# getValidFormats is for internal use only. This function should serve as the "single source of
# truth" with regard to the supported file extensions. All other functions that rely on this list
# simply call this function and use the resulting vector
getValidFormats <- function() {
  return(c("lp", "mod"))
}

#' @keywords internal
isSupportedFormat <- function(filePath) {
  return(tools::file_ext(filePath) %in% getValidFormats())
}

listSampleData <- function() {
  path <- system.file("extdata", package = "readQP")
  list.files(path)
}

#' Get sample data
#' @export
#' @keywords internal
getSampleData <- function(name, isTest = FALSE) {
  if (nchar(tools::file_ext(name)) == 0) {
    name <- paste0(name, ".lp")
  }
  path <- system.file("extdata", name, package = "AlteryxPrescriptive")
  if (!file.exists(path)) {
    msg <- paste(
      "The file you have requested does not exist.",
      "  Maybe you were looking for one these instead?",
      paste("\t", listSampleData(), collapse = "\n"),
      sep = "\n")
    stop(msg)
  }
  return(path)
}

# Helper function for destructuring lists/vectors. Helps when dealing with lists as results.
list <- structure(NA,class="result")
"[<-.result" <- function(x,...,value) {
  args <- as.list(match.call())
  args <- args[-c(1:2,length(args))]
  length(value) <- length(args)
  for(i in seq(along=args)) {
    a <- args[[i]]
    if(!missing(a)) eval.parent(substitute(a <- v,list(a=a,v=value[[i]])))
  }
  x
}
