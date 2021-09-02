#' @title Create correlation heatmap of all samples
#'
#' @description Create a correlation heatmap of all samples..
#'
#' @param lipid_data tibble with all the lipid data
#' @param cent_scale logical, apply center and scaling
#' @param z what to show as intensity of the heatmap
#' @param clust apply clustering yes/no, default is no
#' @param sample_group dataframe with grouping information
#'
#' @return plotly object
#'
#' @importFrom magrittr %>%
#' @importFrom dplyr filter group_by mutate ungroup case_when desc
#' @importFrom rlang .data
#' @importFrom tidyr pivot_wider
#' @importFrom tidyselect matches
#' @importFrom tibble column_to_rownames
#' @import iheatmapr
#'
#' @export
#'
#' @author Rico Derks
#'
compare_samples_heatmap <- function(lipid_data, cent_scale, z, clust = FALSE, sample_group = NULL) {
  lipid_data <- lipid_data %>%
    # only select the samples
    filter(.data$sample_type == "sample",
           .data$keep == TRUE,
           .data$class_keep == TRUE) %>%
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
        z == "raw" ~ .data$area,
        z == "totnorm" ~ .data$norm_area
      ))

  if(cent_scale == TRUE) {
    lipid_data <- lipid_data %>%
      # # scale "row wise" i.e. lipid
      group_by(.data$my_id) %>%
      # keep in mind, scale always returns a matrix
      mutate(plot_z = scale(.data$plot_z)[, 1]) %>%
      ungroup()
  }

  legend_name <- case_when(
    z == "raw" ~ "Raw data",
    z == "totnorm" ~ "Tot. area norm."
  )

  # need to make the data wide and into a matrix
  plot_data <- lipid_data %>%
    pivot_wider(id_cols = c(.data$ShortLipidName, .data$LipidClass),
                names_from = .data$sample_name,
                values_from = .data$plot_z) %>%
    arrange(desc(.data$LipidClass), .data$ShortLipidName) %>%
    column_to_rownames(var = "ShortLipidName") %>%
    select(-.data$LipidClass) %>%
    as.matrix()

  # create the heatmap
  p <- main_heatmap(data = plot_data) %>%
    add_col_labels(textangle = -45) %>%
    add_row_labels()

  if(!is.null(sample_group)){
    # extract the sample group info
    col_groups <- lipid_data %>%
      select(.data$sample_name, any_of(sample_group)) %>%
      distinct(.data$sample_name,
               .keep_all = TRUE) %>%
      select(-.data$sample_name)

    # add coloring groups
    p <- p %>%
      add_col_annotation(col_groups)
  }

  # add clustering
  if(clust == TRUE) {
    p <- p %>%
      add_row_clustering(method = "hclust",
                         side = "right") %>%
      add_col_clustering(method = "hclust")
  }
  return(p)
}
