#' Combinator
#'
#' @importFrom magrittr %>% %T>%
#' @importFrom stringr str_replace
#' @keywords internal
combinator <- function(str, parser) {
  str <- if (class(str) != "combinator") {
    list(i = str)
  } else {
    str
  }
  output <- parser(str$i)
  remaining <- str_replace(str$i, coll(output), '')
  return(structure(list(i = remaining, o = output), class = "combinator"))
}

`%C>%` <- function(lhs, rhs) {
  lhs %>% combinator(rhs)
}

`%O>%` <- function(lhs, rhs) {
  rhs(lhs$o)
}

`%:>%` <- function(lhs, rhs) {
  currentEnv = parent.env(environment())
  lhs %T>% {
    assign(rhs, .$o, envir = currentEnv)
  }
}
