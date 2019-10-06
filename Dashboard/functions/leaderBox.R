
leaderBox <- function (title = "test", 
                       first = "First Place",
                       second = "2nd Place",
                       third = "third",
                       value = NULL, 
                       subtitle = NULL, 
                       icon = shiny::icon("bar-chart"), 
                       color = "aqua", 
                       width = 4, 
                       href = NULL, 
                       fill = FALSE) 
{
  #validateColor(color)
  #tagAssert(icon, type = "i")
  colorClass <- paste0("bg-", color)
  boxContent <- div(class = "info-box", 
                    class = if (fill) colorClass, 
                    span(class = "info-box-icon", 
                         class = if (!fill) colorClass, 
                         icon), 
                    div(class = "info-box-content", 
                        span(class = "info-box-number", title), 
                        if (!is.null(first))  shiny::tags$span(class = "info-box-text", first),
                        if (!is.null(second)) shiny::tags$span(class = "info-box-text", second),
                        if (!is.null(third))  shiny::tags$span(class = "info-box-text", third)
                        ))

  if (!is.null(href)) 
    boxContent <- a(href = href, boxContent)
  div(class = if (!is.null(width)) 
    paste0("col-sm-", width), boxContent)
}
