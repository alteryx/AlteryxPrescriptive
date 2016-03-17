## quiets concerns of R CMD check re: the .'s that appear in pipelines
if(getRversion() >= "2.15.1")  utils::globalVariables(c("."))

#' Parse a quadratic equation into a matrix of coefficients
#'
#' Take a glpk model object a Q equation and return a long-form DF of variable/coef pairs
#'
#' @importFrom stringr str_split str_match
#' @param model An Rglpk model object
#' @param q A string containting the quadratic portion of a CPLEX constraint
#' @return A long matrix of variable pairs and their coefficients
#' @export
parseQ <- function(model, q) {
  vars <- getVarNames(model)
  terms <- tokenizeQ(q)
  coefs <- getCoefNames(terms)
  createVarPairs(vars, coefs)
}

getVarNames <- function(model) {
  attr(model, "objective_vars_names")
}

# Tokenize the QP equation, keeping signs per term
tokenizeQ <- function(qEquation) {
  qEquation %>%
    str_split('(?=[+-])') %>%
    unlist %>% {
      # In order to get the sign on the first term, the tokenizer will split *before* [+-], which
      # means that if the leading sign is "-", we end up with a blank element in the result.
      # This step checks for the blank element and removes it.
      if (nchar(.[1]) == 0) {
        return(.[-1])
      } else {
        return(.)
      }
    } %>%
    # This expression has two marked groups. The first looks for a sign, and some number of digit
    # or non-word characters. This works because variable names cannot START with a number (although
    # they can contain numbers). This also allow us to handle decimal points easily.
    str_match("(^[+-]?[\\d|\\W]*)\\*?(.*$)")
}


getCoefNames <- function(tokenMatrix) {
  coefs <- setNames(tokenMatrix[,2], tokenMatrix[,3])
  # Manually add a coefficient of +/-1 if no coef is specified (e.g x^2, -x^2)
  coefs[coefs %in% c('', '+')] <- '+1'
  coefs[coefs == '-'] <- '-1'
  coefs
}

# Create a long matrix of variable pairs and coefficients
createVarPairs <- function(varList, coefs) {
  varMat <- expand.grid(x1 = varList, x2 = varList, stringsAsFactors = FALSE)
  varMat$x3 <- ifelse(
    varMat$x1 == varMat$x2,
    paste0(varMat$x1, "^2"),
    paste0(varMat$x1, "*", varMat$x2)
  )
  varMat$values <- coefs[varMat$x3]
  varMat
}
