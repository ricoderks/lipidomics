#' @title Create correlation heatmap of all samples
#'
#' @description Create a correlation heatmap of all samples..
#'
#' @param lipid_data tibble with all the lipid data
#' @param z what to show as intensity of the heatmap
#'
#' @return plotly object
#'
#' @importFrom magrittr %>%
#' @importFrom dplyr filter group_by mutate ungroup case_when
#' @importFrom rlang .data
#' @importFrom plotly plot_ly add_heatmap layout
#'
#' @author Rico Derks
#'
compare_samples_heatmap <- function(lipid_data, z) {
  lipid_data <- lipid_data %>%
    # only select the samples
    filter(.data$sample_type == "sample",
           .data$keep == TRUE) %>%
    # scale "row wise" i.e. lipid
    group_by(.data$my_id) %>%
    # keep in mind, scale always returns a matrix
    mutate(scaled_area = scale(.data$area)[, 1]) %>%
    ungroup() %>%
    # total area normalisation
    group_by(.data$sample_name) %>%
    mutate(norm_area = .data$area / sum(.data$area)) %>%
    ungroup() %>%
    # some extra things for plotting
    mutate(
      # order the y-axis according to lipid class and then lipid
      order_yaxis = paste(.data$LipidClass, .data$ShortLipidName, sep = "_"),
      # what to plot
      plot_z = case_when(
        z == "zscore" ~ .data$scaled_area,
        z == "raw" ~ .data$area,
        z == "totnorm" ~ .data$norm_area
      ))

  legend_name <- case_when(
    z == "zscore" ~ "z-score",
    z == "raw" ~ "Raw data",
    z == "totnorm" ~ "Tot. area norm."
  )

  p <- lipid_data %>%
    plot_ly(x = ~sample_name,
            y = ~ShortLipidName,
            colorbar = list(title = legend_name)) %>%
    add_heatmap(z = ~plot_z,
                text = ~LipidClass,
                colorscale = "Rainbow",
                hovertemplate = paste(
                  "%{xaxis.title.text}: %{x}<br>",
                  "%{yaxis.title.text}: %{y}<br>",
                  "Lipid class: %{text}<br>",
                  "Value: %{z:.3f}",
                  "<extra></extra>" # needed to remove the trace box
                ),
                # order the y-axis according to lipid class and then lipid
                yaxis = list(type = "category",
                             categoryorder = "array",
                             categoryarray =  ~order_yaxis)) %>%
    layout(yaxis = list(title = list(text = "Short lipid name")),
           xaxis = list(title = list(text = "Sample name")))

  return(p)
}
