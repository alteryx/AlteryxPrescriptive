#' Helper Functions for Parser
#'
#' @importFrom magrittr %>%
#' @importFrom stringr str_replace_all str_extract
#' @keywords internal
remove_newLines <- function(str) {
  str_replace_all(str, '[\\n\\r\\t]', '')
}

remove_spaces <- function(str) {
  str_replace_all(str, ' ', '')
}

# Extract the model sense
max_min <- function(str) {
  str_extract(str, "(?i)(?:min|max)(?:imize|imum)?")
}

# Pattern for all possible variations of the constraint section
constraintPattern <- "(?i)(?:subject\\s?to|such\\s?that|st|s\\.t\\.|st\\.)"

# Returns everything up to the constraint
get_objective <- function(str) {
  pattern <- paste0("^.+(?=", constraintPattern, ".+$)")
  str_extract(str, pattern)
}

# Returns everything from the constraint onward
getRemainder <- function(file) {
  pattern <- paste0(constraintPattern, ".+$")
  str_extract(file, regex(pattern, dotall = TRUE))
}

# Checks for the existence of a quadratic component in the objective function
check_quadratic <- function(str) {
  str_detect(str, "^.+(?=\\[.+\\])")
}

# Extracts the linear component of the objective function
get_c <- function(str) {
  str_extract(str, "^.+(?=\\[)") %>%
    str_extract("^.+(?=[+-]$)")
}

# Extracts the quadratic component of the objective function
get_Q <- function(str) {
  str_extract(str, "(?<=^[+-]\\[).+(?=\\]/2$)")
}
