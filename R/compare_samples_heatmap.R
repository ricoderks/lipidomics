#' @title Create correlation heatmap of all samples
#'
#' @description Create a correlation heatmap of all samples..
#'
#' @param lipid_data tibble with all the lipid data
#'
#' @return plotly object
#'
#' @importFrom magrittr %>%
#' @importFrom dplyr filter group_by mutate ungroup
#' @importFrom rlang .data
#' @importFrom plotly plot_ly add_heatmap layout
#'
#' @author Rico Derks
#'
compare_samples_heatmap <- function(lipid_data) {
  lipid_data <- lipid_data %>%
    # only select the samples
    filter(.data$sample_type == "sample") %>%
    # scale "row wise" i.e. lipid
    group_by(.data$my_id) %>%
    # keep in mind, scale returns a matrix
    mutate(scaled_area = scale(.data$area)[, 1]) %>%
    ungroup()

  p <- lipid_data %>%
    plot_ly(x = ~sample_name,
            y = ~orgShortLipidName) %>%
    add_heatmap(z = ~scaled_area) %>%
    layout(yaxis = list(title = "Short lipid name"),
           xaxis = list(title = "Sample name"))

  return(p)
}
