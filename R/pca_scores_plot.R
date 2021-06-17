#' @title Make a PCA scores plot
#'
#' @description Make a PCA scores plot with plot_ly
#'
#' @param scores_data tibble containing the scores information
#'
#' @return a plotly graph
#'
#' @importFrom plotly plot_ly add_markers layout
#'
#' @author Rico Derks
#'
pca_scores_plot <- function(scores_data) {
  p <- scores_data %>%
    plot_ly(x = ~PC1,
            y = ~PC2,
            text = ~sample_name) %>%
    add_markers(color = ~sample_type,
                size = 3) %>%
    layout(title = "Scores plot")

  return(p)
}
