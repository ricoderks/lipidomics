#' @title Make a PCA variable plot
#'
#' @description Make a PCA variable plot with plot_ly
#'
#' @param var_data tibble containing the varaible information
#' @param sample_name character vector (length 1) with the sample name
#'
#' @return a plotly graph
#'
#' @importFrom plotly plot_ly add_bars layout hide_legend
#' @importFrom magrittr %>%
#' @importFrom grDevices rainbow
#'
#' @author Rico Derks
#'
pca_variable_plot <- function(var_data, sample_name) {
  # get number of colors needed
  num_colors <- length(unique(var_data$LipidClass))

  # make the plot
  p <- var_data %>%
    plot_ly(x = ~ShortLipidName,
            y = ~value,
            color = ~LipidClass,
            colors = rainbow(n = num_colors),
            text = ~paste0(ShortLipidName, "<br>", LipidClass)) %>%
    add_bars() %>%
    layout(title = list(text = paste("Variable plot", sample_name),
                        x = 0)) %>%
    hide_legend()

  return(p)
}
