#' Transform model into CPLEX LP format
#'
#'
#' @export
#' @param x list of objective, constraints and fieldInfo
makeCPLEXFile <- function(x){
  tpl <- "%s\n  obj: %s\nSubject To\n  %s\nBounds\n  %s\nGeneral\n  %s\nBinary\n  %s\nEnd\n"
  ptype <- if (x$maximize) 'Maximize' else 'Minimize'
  bounds <- paste(sapply(x$fieldList, function(x){
    x$upperBound <- if (is.null(x$upperBound)) "Inf" else x$upperBound
    x$lowerBound <- if (is.null(x$lowerBound)) "0" else x$lowerBound
    sprintf("%s <= %s <= %s", x$lowerBound, x$fieldName, x$upperBound)
  }), collapse = "\n  ")


  obj <- clean_obj(x$objective, x$fieldList)
  constraints <- clean_constr(x$constraints)

  find_type <- function(type){
    f <- Filter(function(x){x$type == type}, x$fieldList)
    paste(sapply(f, '[[', 'fieldName'), collapse = ' ')
  }
  general <- find_type('General')
  binary <- find_type('Binary')
  sprintf(tpl, ptype, obj, constraints, bounds, general, binary)
}

# Helper function that trims "*" from objective

clean_obj <- function(obj, field_list) {
  field_names <- sapply(field_list, function(x){x$fieldName[[1]]})
  block <- paste(field_names, collapse="|")
  pat <- paste0("\\b((?:", block, ")\\*)(?=\\b(?:", block, ")\\b)|\\*")
  gsub(pat, "\\1", obj, perl=T)
}

# Helper function that trims "*", "\n" from constraints
# Using "trim_star" based on the assumption that the constraints are linear
clean_constr <- function(x) {
  trimmed_constr <- sapply(x, trim_newline)
  trimmed_constr <- sapply(trimmed_constr, trim_star)
  constr_names <- paste0("C", seq(1, length(x)))
  named_constr <- ifelse(
    grepl(":", trimmed_constr),
    trimmed_constr,
    paste(constr_names, trimmed_constr, sep = ": ")
  )
  constraints <- paste(named_constr, collapse = "\n  ")
  constraints
}

# trim any "*"
trim_star <- function(x) gsub("\\*", "", x)
trim_newline <- function(x) gsub("^\\n+|\\n+$", "", x)


