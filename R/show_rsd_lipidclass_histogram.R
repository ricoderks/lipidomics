#' @title Show the histogram of all RSD values per lipid class
#'
#' @description Show the histogram of all RSD values of all lipids per lipid class
#'     within all QCpool samples..
#'
#' @param qc_data tibble in tidy format
#' @param lipidclass_ion character vector containing "lipidclass - ion"
#'
#' @return ggplot2 object
#'
#' @importFrom magrittr %>%
#' @importFrom ggplot2 ggplot aes geom_histogram geom_vline scale_fill_manual guides labs theme guide_legend facet_wrap ggtitle
#' @importFrom ggCPM theme_cpm cpm_cols
#' @importFrom rlang .data
#'
#'
#' @author Rico Derks
#'
show_rsd_lipidclass_histogram <- function(qc_data, lipidclass_ion) {
  my_colors <- cpm_cols(c("green", "red"))
  names(my_colors) <- c("pos", "neg")

  p <- qc_data %>%
    filter(.data$class_ion %in% lipidclass_ion) %>%
    ggplot(aes(.data$rsd_area)) +
    geom_histogram(aes(fill = .data$polarity),
                   binwidth = 0.01,
                   alpha = 0.50,
                   position = "identity") +
    geom_vline(aes(xintercept = 0.3,
                   colour = "red"),
               linetype = 2) +
    scale_fill_manual(values = my_colors) +
    guides(colour = FALSE,
           fill = guide_legend(title = "Polarity")) +
    labs(x = "Relative standard deviation",
         y = "Count") +
    ggtitle("RSD per lipidclass") +
    facet_wrap(~ .data$LipidClass,
               scales = "free_y",
               ncol = 4) +
    theme_cpm() +
    theme(legend.position = "bottom")

  return(p)
}
