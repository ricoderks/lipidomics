#' @title Show a violin plot of all RSD values per lipid class
#'
#' @description Show a violin plot of all RSD values of all lipids per lipid class
#'     within all QCpool samples..
#'
#' @param qc_data tibble in tidy format
#' @param rsd rsd threshold to show in the plot
#'
#' @return ggplot2 object
#'
#' @importFrom magrittr %>%
#' @importFrom ggplot2 ggplot aes geom_violin geom_hline scale_colour_manual guides labs theme guide_legend facet_wrap ggtitle coord_flip geom_jitter
#' @importFrom ggCPM theme_cpm cpm_cols
#' @importFrom rlang .data
#'
#' @export
#'
#' @author Rico Derks
#'
show_rsd_lipidclass_violin <- function(qc_data, rsd) {
  my_colors <- cpm_cols(c("green", "red"))
  names(my_colors) <- c("pos", "neg")

  p <- qc_data %>%
    ggplot(aes(x = .data$LipidClass,
               y = .data$rsd_area)) +
    geom_violin(scale = "width") +
    geom_jitter(aes(colour = .data$polarity),
                alpha = 0.5) +
    geom_hline(yintercept = rsd,
               colour = "red",
               linetype = 2) +
    scale_colour_manual(values = my_colors) +
    guides(colour = guide_legend(title = "Polarity")) +
    labs(y = "Relative standard deviation",
         x = "Lipid class") +
    ggtitle("RSD per lipidclass") +
    coord_flip() +
    theme_cpm() +
    theme(legend.position = "bottom")

  return(p)
}
