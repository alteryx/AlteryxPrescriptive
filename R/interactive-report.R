#' Make variable report
#'
#' @param out output from AlteryxSolve
#' @export
makeVariableReport <- function(out){
  d <- data.frame(
    Variable = out$lp_attr$objective_vars_names,
    Value = signif(out$solution, 3),
    Coefficient = as.vector(out$OP$objective$L),
    Type = ROI::types(out$OP),
    stringsAsFactors = FALSE
  )
  d <- d[order(-d$Value),]
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
  d <- d[order(abs(d$slack)),]
  d
}

#' Make interactive report
#'
#' @param out output from AlteryxSolve
#' @param nOutput output anchor to send html output to
#' @param ... other arguments to pass to renderInComposer
#' @import DT
#' @export
makeInteractiveReport <- function(out, nOutput = 3, ...){
  requireNamespace("AlteryxRviz", quietly = TRUE)
  tour = AlteryxRviz::intro(list(
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

  styleTable <- htmlwidgets::JS(
    "function(settings, json) {
        $(this.api().table().header()).css({
          'background-color': '#14a99d',
          'color': 'white'
        });
    }"
  )
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
      rownames = FALSE,
      filter = list(position = 'top', plain = TRUE),
      options = list(
        pageLength = 5,
        initComplete = styleTable,
        escape = FALSE
      )
    ) %>%
    formatSignif("Value") %>%
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
      rownames = FALSE,
      filter = list(position = 'top', plain = TRUE),
      options = list(
        initComplete = styleTable,
        escape = FALSE,
        pageLength = 5
      )
    ) %>%
    formatSignif("activity", 3) %>%
    formatSignif("slack", 3) %>%
    formatStyle('activity',
      background = styleColorBar(d4$activity, 'steelblue'),
      backgroundSize = '100% 90%',
      backgroundRepeat = 'no-repeat',
      backgroundPosition = 'center'
    ) %>%
    formatStyle('slack',
      color = JS("Math.abs(value) > 0 ? 'green' : 'gray'")
    )

  # title1 = tags$span(
  #   tags$span("Optimal Solution"),
  #   tags$span(class = "label label-success", out$objval)
  # )
  title1 = AlteryxRviz::panel_title("Optimal Solution", "These are variables", 'tooltip1b')
  panel1 = AlteryxRviz::Panel(c(12, 6),
    tags$div(class = 'wrapper',
      tags$p(class = "lead",
        tags$span("Objective Function Value: "),
        tags$span(class = 'label label-success', out$objval)
      ),
      d3
    ),
    title1, id = 'variables'
  )

  title2 = AlteryxRviz::panel_title("Constraints", "These are constraints", "tooltip2b")
  panel2 = AlteryxRviz::Panel(c(12, 6), tags$div(class = 'wrapper', d5),
    title2, id = 'constraints'
  )

  objective_function = list(
    Solution = list(
      value = out$objval,
      title = 'Optimal Value',
      definition = "<p>This is the optimal value</p>"
    )
  )
  solPanel <-  Panel(
    c(12, 4),
    AlteryxRviz::infobox(objective_function, div = 'col-xs-12 col-md-4'),
    'Objective Value'
  )
  iout <- AlteryxRviz::keen_dash(
    AlteryxRviz::Navbar('Optimization',
      AlteryxRviz::navItem(AlteryxRviz::icon('play'), 'Tour', href='#')
    ),
    AlteryxRviz::Row(panel1),
    AlteryxRviz::Row(panel2),
    tour,
    activatePopup(),
    tags$style("
      .wrapper{margin-left: 20px; margin-right: 20px;}
      table.dataTable thead th, table.dataTable thead td{
        border-bottom: 0;
      }
    ")
  )
  AlteryxRviz::renderInComposer(iout, nOutput = nOutput, ...)
}








