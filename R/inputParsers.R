# Helper functions for Parsing Inputs for the Report
# a dataframe:
#    x1   x2   x3
#     1   -4    5
# will be parsed to a string:
#    "x1 - 4*x2 + 5*x3"
parse_helper <- function(df, col_names = names(df)){
  out <- c()
  n <- length(df)
  for (i in 1:n) {
    the_sign <- if (df[[i]] > 0) "+" else "-"
    if (abs(df[[i]]) != 1) {
      if (df[[i]] != 0) {
        if (length(out)) {
          out <- sprintf("%s %s %s*%s", out, the_sign, abs(df[[i]]), col_names[i])
        } else{
          out <- append(out, paste(df[[i]], col_names[i], sep = '*'))
        }
      }
    } else {
      if (length(out)) {
        out <- sprintf("%s %s %s", out, the_sign, col_names[i])
      } else {
        if (the_sign == "+") {
          out <- append(out, col_names[i])
        } else {
          out <- append(out, paste0(the_sign, col_names[i]))
        }
      }
    }
  }
  return(out)
}

parse_objective <- function(df, dir) {
  return(paste(dir, parse_helper(df)))
}

parse_constraints <- function(x) {
  # x: the return from Rglpk_read_file, then
  # constraints's type is: x$constraints
  dff <- as.data.frame(x$constraints[[1]]$i)
  dff <- cbind(dff, x$constraints[[1]]$j)
  dff <- cbind(dff, x$constraints[[1]]$v)
  colnames(dff) <- c('i','j','v')

  var_names <- attr(x, "objective_vars_names")
  start <- dff$i[1]
  end <- dff$i[nrow(dff)]
  df <- data.frame()
  for (i in start:end) {
    dr <- dff[dff$i == i, ]
    dt <- as.data.frame(t(dr$v))
    colnames(dt) <- var_names[dr$j]
    dtt <- as.data.frame(parse_helper(dt), stringsAsFactors = FALSE)
    df <- rbind(df, dtt)
  }
  colnames(df) <- 'eqn'

  df <- cbind(df, dir = x$constraints[[2]], rhs = x$constraints[[3]])
  out <- apply(df, 1, paste, collapse = " ")
  return(as.data.frame(out))
}

create_bt <- function(x) {
  df <- as.data.frame(t(x$bounds$lower$val))
  df <- rbind(df, x$bounds$upper$val)
  df <- rbind(df, x$types)
  colnames(df) <- attr(x, "objective_vars_names")
  return(df)
}

parse_bt <- function(df) {
  stop.Alteryx <- getFromNamespace('stop.Alteryx', 'AlteryxRDataX')
  out <- c()
  for (x in colnames(df)) {
    if (df[[x]][3] == "I") {
      type <- "Integer"
    } else if (df[[x]][3] == "B") {
      type <- "Binary"
    } else if (df[[x]][3] == "C") {
      type <- "Continuous"
    } else {
      stop.Alteryx("Error: Decision variables have unrecognizable types.
                   Currently, Alteryx only supports C(Continuous), I(Integer), B(Binary)")
    }
    a <- sprintf("%s <= %s <= %s, %s is: %s", df[[x]][1], x, df[[x]][2], x, type)
    out <- append(out, a)
    }
  return(as.data.frame(out))
}
