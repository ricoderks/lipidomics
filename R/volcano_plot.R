#' @title Create volcano plot
#'
#' @description Create volcano plot.
#'
#' @param lipid_data tibble with all the lipid data and test data
#'
#' @return plotly object
#'
#' @importFrom magrittr %>%
#' @importFrom plotly plot_ly add_markers layout
#'
#' @author Rico Derks
#'
volcano_plot <- function(lipid_data) {
  p <- lipid_data %>%
    plot_ly(x = ~fc_log2,
            y = ~p_log10) %>%
    add_markers() %>%
    layout(xaxis = list(zeroline = FALSE),
           #yaxis = list(zeroline = FALSE),
           shapes = list(vline(-1),
                         vline(1),
                         hline(-log10(0.05))))

  return(p)
}

vline <- function(x = 0, color = "blue") {
  list(
    type = "line",
    y0 = 0,
    y1 = 1,
    yref = "paper",
    x0 = x,
    x1 = x,
    line = list(color = color,
                width = 1,
                dash = "dash")
  )
}

hline <- function(y = 0, color = "blue") {
  list(
    type = "line",
    x0 = 0,
    x1 = 1,
    xref = "paper",
    y0 = y,
    y1 = y,
    line = list(color = color,
                width = 1,
                dash = "dash")
  )
}
