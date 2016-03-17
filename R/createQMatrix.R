#' Create Q Matrix
#'
#' @param coefs coefficients
#' @importFrom slam as.simple_triplet_matrix
#' @export
createQMatrix <- function(coefs) {
  coefsVec <- as.numeric(coefs$values)
  qMat <- vecToMat(coefsVec)
  return(as.simple_triplet_matrix(qMat))
}

vecToMat <- function (v) {
  v[is.na(v)] <- 0
  n <- sqrt(length(v))
  if (n != round(n,1)) {
    stop("The vector you provided is not the correct size")
  }
  m <- matrix(v, n, n)
  1/2*(m+t(m))
}
