#' Process QP
#'
#'
#' @param file file
#' @param type type
#' @importFrom magrittr %>% %T>%
#' @importFrom stringr str_extract str_replace regex coll
#' @importFrom Rglpk Rglpk_read_file
#' @export
processQP <- function(file, type) {
  comps <- getModelComponents(file)
  tmp.path <- rebuildLP(comps)
  tmp.model <- Rglpk_read_file(tmp.path, type = type)
  res <- list(model = tmp.model, vecMap = parseQ(tmp.model, comps$qp_obj))
  return(res)
}

isQP <- function(file) {
  file %>%
    remove_newLines %>%
    remove_spaces %C>%
    max_min %C>%
    get_objective %O>%
    check_quadratic %>% {
      if (is.na(.)) {
        return(FALSE)
      }
      return(.)
    }
}

getGoalAndObj <- function(file) {
  res <- list()
  file %>%
    remove_newLines %>%
    remove_spaces %C>%
    max_min %T>% {
      .$o ->> res$goal
    } %C>%
    get_objective %>% {
      fullObj <- .$o
      fullObj %C>%
        get_c %T>% {
          str_replace(fullObj, coll(.$i), '') ->> res$lp_obj
        } %C>%
        get_Q %>% {
          .$o ->> res$qp_obj
        }
    }
  return(res)
}

getModelComponents <- function(file) {
  tmp.rem <- getRemainder(file)
  res <- getGoalAndObj(file)
  res$rem <- tmp.rem
  return(res)
}

rebuildLP <- function(comps) {
  tmpContents <- paste(comps$goal, comps$lp_obj, comps$rem, sep = "\n")
  tmpFile <- tempfile("tmp_lp", fileext = ".lp")
  writeLines(tmpContents, tmpFile)
  return(tmpFile)
}
