#' @title Make a PCA observation plot
#'
#' @description Make a PCA obaservation plot with plot_ly
#'
#' @param obs_data tibble containing the varaible information
#' @param var_name character vector of length 1 with the name of the variable
#' @param color_by color the observation by what
#'
#' @return a plotly graph
#'
#' @importFrom plotly plot_ly add_bars layout hide_legend
#' @importFrom dplyr mutate
#' @importFrom magrittr %>%
#' @importFrom grDevices rainbow
#'
#' @author Rico Derks
#'
pca_observation_plot <- function(obs_data, var_name, color_by = "none") {
  if(color_by != "none") {
    obs_data$color_group <- obs_data[[color_by]]
  } else {
    obs_data$color_group <- color_by
  }

  # make the plot
  p <- obs_data %>%
    plot_ly(x = ~sample_name,
            y = ~value,
            color = ~color_group) %>%
    add_bars() %>%
    layout(title = list(text = paste("Observation plot for", var_name),
                        x = 0)) %>%
    hide_legend() %>%
    config(displayModeBar = FALSE)

  return(p)
}
