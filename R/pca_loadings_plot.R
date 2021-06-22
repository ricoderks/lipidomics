#' @title Make a PCA loadings plot
#'
#' @description Make a PCA loadings plot with plot_ly
#'
#' @param loadings_data tibble containing the loadings information
#' @param xaxis which component to show on the x-axis (default = "PC1")
#' @param yaxis which component to show on the y-axis (default = "PC2")
#'
#' @return a plotly graph
#'
#' @importFrom plotly plot_ly add_markers layout hide_legend config
#' @importFrom dplyr mutate
#' @importFrom stringr str_extract
#' @importFrom magrittr %>%
#' @importFrom tidyr pivot_wider
#' @importFrom rlang .data
#' @importFrom grDevices rainbow
#'
#' @author Rico Derks
#'
pca_loadings_plot <- function(loadings_data, xaxis = "PC1", yaxis = "PC2") {
  # what to show on the axes
  loadings_data$show_x <- loadings_data[[xaxis]]
  loadings_data$show_y <- loadings_data[[yaxis]]

  num_colors <- length(unique(loadings_data$LipidClass))

  p <- loadings_data %>%
    plot_ly(x = ~show_x,
            y = ~show_y,
            color = ~LipidClass,
            colors = rainbow(n = num_colors),
            text = ~paste0(ShortLipidName, "<br>", LipidClass),
            customdata = loadings_data$ShortLipidName,
            source = "pca_loadings_plot") %>%
    add_markers(size = 3) %>%
    layout(title = list(text = "Loadings plot",
                        x = 0),
           xaxis = list(title = xaxis),
           yaxis = list(title = yaxis)) %>%
    hide_legend() %>%
    event_register(event = "plotly_click")

  return(p)
}
