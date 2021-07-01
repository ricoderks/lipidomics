#' @title Create box plot
#'
#' @description Create box plot.
#'
#' @param lipid_data tibble with all the lipid data and test data
#' @param title title of the plot
#'
#' @return plotly object
#'
#' @importFrom magrittr %>%
#' @importFrom dplyr filter select
#' @importFrom tidyr unnest
#' @importFrom rlang .data
#' @importFrom plotly plot_ly layout hide_legend
#'
#' @author Rico Derks
#'
box_plot <- function(lipid_data, title = "") {
  # create the plot
  p <- lipid_data %>%
    plot_ly(x = ~my_group_info,
            y = ~area,
            text = ~sample_name,
            color = ~my_group_info,
            type = "box",
            boxpoints = "all",
            jitter = 0.4,
            pointpos = 0) %>%
    layout(title = list(text = title,
                        x = 0),
           yaxis = list(title = "Value"),
           xaxis = list(title = "Group")) %>%
    hide_legend()

  return(p)
}
