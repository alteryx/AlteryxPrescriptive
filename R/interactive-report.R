#' Make variable report
#'
#' @param out output from AlteryxSolve
#' @export
makeVariableReport <- function(out){
  if (out$status$code == 1){
    return(getStatusMessage(out))
  }
  typs <- ROI::types(out$OP)
  d <- data.frame(
    Variable = out$lp_attr$objective_vars_names,
    Value = out$solution,
    Coefficient = as.vector(out$OP$objective$L),
    Type = if (is.null(typs)) "C" else typs,
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
    Constraint = out$lp_attr$constraint_names,
    Value = out$row_activity$optimals,
    Direction = constraints(out$OP)$dir,
    RHS = constraints(out$OP)$rhs,
    Slack = out$row_activity$slacks
  )
  d <- d[order(abs(d$Slack)),]
  d
}

#' Make interactive report
#'
#' @param out output from AlteryxSolve
#' @param nOutput output anchor to send html output to
#' @param ... other arguments to pass to renderInComposer
#' @import htmltools htmlwidgets AlteryxRviz jsonlite DT
#' @export
makeInteractiveReport <- function(out, nOutput = 3, ...){
  requireNamespace("AlteryxRviz", quietly = TRUE)
  if (out$status$code == 1){
    iout <- keen_dash(
      Navbar(paste('Optimization -', out$status$msg$message),
        navItem(icon('play'), 'Tour', href='#')
      )
    )
    renderInComposer(iout, nOutput = nOutput, ...)
    return()
  }
  myTableClass = 'table table-condensed table-striped'
  noteMain = "This interactive dashboard provides a summary of the optimization problem and the optimal solution."
  noteSummary = "This table summarizes the optimization problem and the optimal solution. It displays summary statistics about the problem as well as the optimal value of the objective function."
  noteVar = "This table shows the decision variables and their final values in the optimal solution. Use the search boxes to filter the table based on a column."
  noteConstraints = "This tables shows the constraints and their final values in the optimal solution. Use the search boxes to filter the table based on a column"
  footNote = "Use the search boxes below the headers to filter the table based on the values in a column."
  tour = intro(list(
    list(intro = noteMain),
    list(
      intro = noteSummary,
      element = JS("document.querySelector('#summary')"),
      numberPosition = 'right'
    ),
    list(
      intro = noteVar,
      element = JS("document.querySelector('#variables')"),
      numberPosition = 'left'
    ),
    list(
      intro = noteConstraints,
      element = JS("document.querySelector('#constraints')"),
      position = "auto"
    )
  ), list(), button = '.navbar li>a', width = 90, height = 30)

  styleTable <- JS(
    "function(settings, json) {
        $(this.api().table().header()).css({
          'background-color': '#14a99d',
          'color': 'white'
        });
    }"
  )
  activatePopup <- function(){
    tags$script("
    $(document).ready(function(){
      $('[data-toggle=popover]').popover()
        document.querySelector('body div').style['font-size'] = null
      })
    ")
  }
  d2 <- makeVariableReport(out)
  d3 <- d2 %>%
    datatable(
      class = myTableClass,
      rownames = FALSE,
      filter = list(position = 'top', plain = TRUE),
      options = list(
        info = F,
        sDom = 'tip',
        pageLength = 5,
        #initComplete = styleTable,
        escape = FALSE
      ),
      width = '100%',
      height = 'auto'
    ) %>%
    #formatSignif("Value") %>%
    DT::formatStyle('Value',
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
      class = myTableClass,
      rownames = FALSE,
      filter = list(position = 'top', plain = TRUE),
      options = list(
        info = F,
        sDom = 'tip',
        #initComplete = styleTable,
        escape = FALSE,
        pageLength = 5
      ),
      width = '100%',
      height = 'auto'
    ) %>%
    #DT::formatSignif("Value", 3) %>%
    #DT::formatSignif("Slack", 3) %>%
    # formatStyle('Value',
    #   background = styleColorBar(d4$Value, 'steelblue'),
    #   backgroundSize = '100% 90%',
    #   backgroundRepeat = 'no-repeat',
    #   backgroundPosition = 'center'
    # ) %>%
    formatStyle('Slack',
      background = JS("Math.abs(value) > 0 ? 'white' : 'lightpink'")
    )

  title1 = panel_title("Decision Variables", noteVar, 'tooltip1b')
  panel1 = Panel(c(12, 8), d3,
    title1, footNote, id = 'variables'
  )
  A = getProblemSummary(out)
  A[1,'Value'] = sprintf(
    "<span class='label label-success' style='font-size:medium;'>%s</span>", A[1,'Value']
  )
  summaryReport <- datatable(
    class = myTableClass,
    A,
    rownames = FALSE,
    escape = FALSE,
    options = list(
      sDom = 't',
      bSort = F
    ),
    width = '100%',
    height = 'auto'
  )
  optimText <- commonmark::markdown_html(capture.output(out$OP)[-1])
  panel1a = Panel(c(12, 4),
    summaryReport,
    panel_title('Solution Summary', optimText, 'dummy'),
    id = "summary"
  )
  title2 = panel_title(
    "Constraints", noteConstraints, "tooltip2b"
  )
  panel2 = Panel(c(12, 12), tags$div(class = 'wrapper', d5),
    title2, footNote, id = 'constraints'
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
    infobox(objective_function, div = 'col-xs-12 col-md-4'),
    'Objective Value'
  )
  iout <- keen_dash(
    Navbar('Optimization',
      navItem(icon('play'), 'Tour', href='#')
    ),
    Row(panel1a, panel1),
    Row(panel2),
    tour,
    activatePopup(),
    tags$style("
      /* .wrapper{margin-left: 20px; margin-right: 20px;} */
      table.dataTable thead th, table.dataTable thead td{
        border-bottom: 0;
      }
      div.DTS div.dataTables_scrollBody {
        background: none;
      }
    ")
  )
  renderInComposer(iout, nOutput = nOutput, ...)
}

#' Get problem summary
#'
#'
#' @param out object returned by AlteryxSolve
#' @export
getProblemSummary <- function(out){
  data.frame(
    Description = c('Objective Value', 'Problem Type', 'Objective',
      'Number of Variables', 'Integer Variables (including Binary)',
      'Binary Variables', 'Number of Constraints',
      'Number of Nonzero Coefficients'
    ),
    Value = c(
      out$objval,
      getProblemType(out),
      if (out$OP$maximum) 'Maximize' else 'Minimize',
      out$lp_attr$n_objective_vars,
      out$lp_attr$n_integer_vars,
      out$lp_attr$n_binary_vars,
      out$lp_attr$n_constraints,
      out$lp_attr$n_nonzeros
    ),
    stringsAsFactors = F
  )
}

#' Make data output
#'
#'
#' @param out object returned by AlteryxSolve
#' @param format pipe, json, simple or list
#' @importFrom plyr ldply
#' @export
makeDataOutput <- function(out, format = 'pipe'){
  if (out$status$code == 1){
    return(getStatusMessage(out))
  }
  d1 = list(
    summary = getProblemSummary(out),
    variables = makeVariableReport(out),
    constraints = makeConstraintReport(out)
  )
  if (format == "json"){
    data.frame(
      name = names(d1),
      value = sapply(unname(d1), function(x){
        jsonlite::toJSON(x)
      })
    )
  } else if (format == 'pipe'){
    ldply(names(d1), function(k){
      data.frame(name = k, value = df2pipe(d1[[k]]))
    })
  } else if (format == 'simple') {
    d2 <- setNames(d1$summary[1,], c('name', 'value'))
    d3 <- setNames(d1$variables[,1:2], c('name', 'value'))
    rbind(d2, d3)
  } else {
    d1
  }
}

#' @importFrom stats setNames terms
#' @importFrom utils capture.output write.table
df2pipe <- function(df, quote = F, ...){
  capture.output(write.table(df, sep = "|", quote = quote, row.names = F, ...))
}


#' Detect problem type based on class of OP object and types of variables
#'
#'
#' @param out object returned by AlteryxSolve
#' @export
getProblemType <- function(out){
  pType <- if (inherits(out$OP, 'quadprog')){
    'QP'
  } else if (out$lp_attr$n_integer_vars == 0){
    'LP'
  } else if (out$lp_attr$n_objective_vars > out$lp_attr$n_integer_vars){
    'MILP'
  } else {
    'IP'
  }
}

getStatusMessage <- function(out){
  x <- unclass(out$status$msg)
  d <- data.frame(name = names(x), value = unname(sapply(x, '[[', 1)))
  return(d)
}
