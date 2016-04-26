#' Transform model into CPLEX LP format
#'
#'
#' @export
#' @param x list of objective, constraints and fieldInfo
makeCPLEXFile <- function(x){
  tpl <- "%s\n  obj: %s\nSubject To\n  %s\nBounds\n  %s\nGeneral\n  %s\nBinary\n  %s\nEnd\n"
  ptype = if (x$maximize) 'Maximize' else 'Minimize'
  bounds = paste(sapply(x$fieldList, function(x){
    x$upperBound = if (is.null(x$upperBound)) "+inf" else x$upperBound
    sprintf("%s <= %s <= %s", x$lowerBound, x$fieldName, x$upperBound)
  }), collapse = "\n  ")
  constraints = paste(x$constraints, collapse = "\n  ")
  find_type <- function(type){
    f <- Filter(function(x){x$type == type}, x$fieldList)
    paste(sapply(f, '[[', 'fieldName'), collapse = '\n  ')
  }
  general = find_type('General')
  binary = find_type('Binary')
  sprintf(tpl, ptype, x$objective, constraints, bounds, general, binary)
}
