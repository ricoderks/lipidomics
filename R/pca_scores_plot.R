#' @title Make a PCA scores plot
#'
#' @description Make a PCA scores plot with plot_ly
#'
#' @param scores_data tibble containing the scores information
#' @param xaxis which component to show on the x-axis (default = "PC1")
#' @param yaxis which component to show on the y-axis (default = "PC2")
#'
#' @return a plotly graph
#'
#' @importFrom plotly plot_ly add_markers layout
#' @importFrom magrittr %>%
#'
#' @author Rico Derks
#'
pca_scores_plot <- function(scores_data, xaxis = "PC1", yaxis = "PC2") {
  scores_data$show_x <- scores_data[[xaxis]]
  scores_data$show_y <- scores_data[[yaxis]]

  p <- scores_data %>%
    plot_ly(x = ~show_x,
            y = ~show_y,
            text = ~sample_name) %>%
    add_markers(color = ~sample_type,
                size = 3) %>%
    layout(title = "Scores plot",
           xaxis = list(title = xaxis),
           yaxis = list(title = yaxis))

  return(p)
}
