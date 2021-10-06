#' @title Create bubble plot
#'
#' @description Create a bubble plot.
#'
#' @param lipid_data tibble with all the lipid data and test data
#' @param pattern regular expression used to select the correct lipids
#' @param title title of the plot
#'
#' @details This bubble plot function is used to create the bubble plots in the
#'     report.
#'
#' @return ggplot object
#'
#' @importFrom ggplot2 ggplot aes geom_point scale_size geom_line geom_text facet_grid labs guides coord_cartesian theme
#' @importFrom ggCPM theme_cpm
#'
#' @export
#'
#' @author Rico Derks
#'
bubble_plot <- function(lipid_data, pattern = NULL, title = "") {
  # get the sample_name of the first qcpool sample
  selected_name <- lipid_data %>%
    filter(grepl(x = .data$sample_type,
                 pattern = "[qQ][cC][pP][oO][oO][lL]")) %>%
    arrange(.data$sample_name) %>%
    distinct(.data$sample_name) %>%
    slice(1) %>%
    pull(.data$sample_name)

  if(length(selected_name) == 0) {
    # if no QCpool is in the dataset select the first sample
    selected_name <- lipid_data %>%
      filter(grepl(x = .data$sample_type,
                   pattern = "[sS][aA][mM][pP][lL][eE]")) %>%
      arrange(.data$sample_name) %>%
      distinct(.data$sample_name) %>%
      slice(1) %>%
      pull(.data$sample_name)
  }

  # get the data to be plotted
  plot_data <-  lipid_data %>%
    filter(.data$sample_name == selected_name,
           grepl(x = .data$LipidClass,
                 pattern = pattern),
           # show only lipid class which are not discarded
           .data$class_keep == TRUE,
           # show only lipids which have a low enough RSD
           .data$rsd_keep == TRUE)

  # create the plot
  if(nrow(plot_data) > 0) {
    p <- plot_data %>%
      ggplot(aes(x = .data$AverageRT,
                 y = .data$AverageMZ,
                 color = .data$carbons)) +
      geom_point(aes(size = .data$DotProduct),
                 alpha = 0.4) +
      # show lipid which already should be discarded as grey
      geom_point(data = plot_data[plot_data$keep == FALSE, ],
                 aes(size = .data$DotProduct),
                 color = "grey",
                 alpha = 1) +
      scale_size(range = c(1, 10),
                 limits = c(0, 100)) +
      geom_line() +
      geom_text(aes(label = .data$carbon_db),
                size = 3.0,
                color = "black") +
      facet_grid(.data$LipidClass ~ .data$ion,
                 scales = "free") +
      labs(x = "Retention time [minutes]",
           y = expression(italic("m/z"))) +
      guides(color = "none",
             size = "none") +
      theme_cpm() +
      theme(strip.text = element_text(size = 10))
  } else {
    p <- NULL
  }

  return(p)
}
