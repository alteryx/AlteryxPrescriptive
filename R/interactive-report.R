#' Make variable report
#'
#' @param out output from AlteryxSolve
#' @export
makeVariableReport <- function(out){
  d <- data.frame(
    Variable = out$lp_attr$objective_vars_names,
    Coefficient = as.vector(out$OP$objective$L),
    Value = out$solution,
    stringsAsFactors = FALSE
  )
  d <- d[order(d$Value),]
  if (!is.null(types(out$OP)) && all(types(out$OP) == "B")){
    d <- d[d$Value == 1,]
  }
  d
}


#' Make constraint report
#'
#' @param out output from AlteryxSolve
#' @export
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
#' @import DT
#' @import AlteryxRviz
#' @export
makeInteractiveReport <- function(out, nOutput = 3, ...){
  tour = intro(list(
    list(intro = 'Optimization'),
    list(
      intro = "Variables",
      element = JS("document.querySelector('#variables')"),
      numberPosition = 'right',
      position = "right"
    ),
    list(
      intro = 'Constraints',
      element = JS("document.querySelector('#constraints')"),
      numberPosition = 'right',
      position = "right"
    )
  ), list(), button = '.navbar li>a', width = 90, height = 30)

  activatePopup <- function(){
    htmltools::tags$script("
    $(document).ready(function(){
      $('[data-toggle=popover]').popover()
        document.querySelector('body div').style['font-size'] = null
      })
    ")
  }
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
  title1 = panel_title("Variables", "These are variables", 'tooltip1b')
  panel1 = Panel(c(12, 8), d3, title1, id = 'variables')

  title2 = panel_title("Constraints", "These are constraints", "tooltip2b")
  panel2 = Panel(c(12, 8), d5, title2, id = 'constraints')
  iout <- keen_dash(
    Navbar('Optimization',
      navItem(icon('play'), 'Tour', href='#')
    ),
    Row(panel1),
    Row(panel2),
    tour,
    activatePopup()
  )
  renderInComposer(iout, nOutput = nOutput, ...)
}








