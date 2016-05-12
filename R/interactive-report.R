makeVariableReport <- function(out){
  d <- data.frame(
    Variable = out$lp_attr$objective_vars_names,
    Coefficient = as.vector(out$OP$objective$L),
    Value = out$solution,
    stringsAsFactors = FALSE
  )
  d <- plyr::arrange(d, -Value)
  if (!is.null(types(out$OP)) && all(types(out$OP) == "B")){
    d <- d[d$Value == 1,]
  }
  d
}



makeConstraintReport <- function(out){
  d <- data.frame(
    constraint = out$lp_attr$constraint_names,
    activity = out$row_activity$optimals,
    dir = constraints(out$OP)$dir,
    rhs = constraints(out$OP)$rhs,
    slack = out$row_activity$slacks
  )
  d
}

#' Make interactive report
#'
#' @param out output from AlteryxSolve
#' @import AlteryxRviz
#' @export
makeInteractiveReport <- function(out){
  library(magrittr)
  library(AlteryxRviz)
  library(DT)
  d2 <- makeVariableReport(out)
  d3 <- d2 %>%
    datatable(
      filter = list(position = 'top', plain = TRUE),
      options = list(pageLength = 5),
      escape = FALSE
    ) %>%
    formatStyle('Value',
      background = styleColorBar(d2$Value, 'steelblue'),
      backgroundSize = '100% 90%',
      backgroundRepeat = 'no-repeat',
      backgroundPosition = 'center'
    ) %>%
    htmlwidgets::onRender("function(el, x){
      $('[data-toggle=tooltip]').tooltip({
        animated: 'fade',
        placement: 'right',
        html: true
      })
    }")
  d4 <- makeConstraintReport(out)
  d5 <- d4 %>%
    datatable(
      filter = list(position = 'top', plain = TRUE),
      options = list(pageLength = 5)
    ) %>%
    formatStyle('activity',
      background = styleColorBar(d4$activity, 'steelblue'),
      backgroundSize = '100% 90%',
      backgroundRepeat = 'no-repeat',
      backgroundPosition = 'center'
    ) %>%
    formatStyle('slack',
      color = JS("Math.abs(value) > 0 ? 'green' : 'gray'")
    )
  keen_dash(
    Navbar2('Optimization'),
    Row(
      Panel(c(12, 8), d3, 'Optimal Solution')
    ),
    Row(
      Panel(c(12, 8), d5, 'Optimal Solution')
    )
  )
}








