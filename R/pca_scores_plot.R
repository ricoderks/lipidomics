#' @title Make a PCA scores plot
#'
#' @description Make a PCA scores plot with plot_ly
#'
#' @param scores_data tibble containing the scores information
#' @param xaxis which component to show on the x-axis (default = "PC1")
#' @param yaxis which component to show on the y-axis (default = "PC2")
#' @param color_by color the observation by this group
#'
#' @return a plotly graph
#'
#' @importFrom plotly plot_ly add_markers layout config event_register
#' @importFrom magrittr %>%
#'
#' @author Rico Derks
#'
pca_scores_plot <- function(scores_data, xaxis = "PC1", yaxis = "PC2", color_by = "none") {
  # select what to show on the axis
  scores_data$show_x <- scores_data[[xaxis]]
  scores_data$show_y <- scores_data[[yaxis]]

  # is there coloring
  if(color_by == "none") {
    scores_data$color_by <- scores_data[["sample_type"]]
  } else {
    scores_data$color_by <- scores_data[[color_by]]
  }

  p <- scores_data %>%
    plot_ly(x = ~show_x,
            y = ~show_y,
            text = ~sample_name,
            customdata = scores_data$sample_name,
            source = "pca_scores_plot") %>%
    add_markers(color = ~color_by,
                size = 3) %>%
    layout(title = list(text = "Scores plot",
                        x = 0),
           xaxis = list(title = xaxis),
           yaxis = list(title = yaxis)) %>%
    # config(displayModeBar = FALSE) %>%
    event_register(event = "plotly_click")

  return(p)
}
