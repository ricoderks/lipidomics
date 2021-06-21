#' @title Make an explained variance plot
#'
#' @description Make an explained variance plot for PCA with plotly
#'
#' @param exp_var_data tibble containing the scores information
#' @param num_pc number of components to plot, default is 5
#'
#' @return a plotly graph
#'
#' @importFrom plotly plot_ly add_bars layout config
#' @importFrom magrittr %>%
#' @importFrom rlang .data
#'
#' @author Rico Derks
#'
pca_explained_var_plot <- function(exp_var_data, num_pc = 5) {

  p <- exp_var_data %>%
    filter(.data$component %in% paste0("PC", 1:num_pc)) %>%
    plot_ly(x = ~component,
            y = ~cum_exp_var) %>%
    add_bars() %>%
    layout(title = "Explained variance",
           xaxis = list(title = "Principal components"),
           yaxis = list(title = "Cum. explained variance [%]")) %>%
    config(displayModeBar = FALSE)

  return(p)
}
