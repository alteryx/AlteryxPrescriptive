## Helper Functions ----
#' Fix Slam Matrix
#'
#' @importFrom slam is.simple_triplet_matrix
#' @keywords internal
fixSlamMatrix <- function(m){
  if (slam::is.simple_triplet_matrix(m)){
    ord = sort.int(m$j, index.return = TRUE)
    m$i = m$i[ord$ix]
    m$j = m$j[ord$ix]
    m$v = m$v[ord$ix]
    return(m)
  } else {
    return(m)
  }
}
