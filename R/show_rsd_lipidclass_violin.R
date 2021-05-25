#' @title Show a violin plot of all RSD values per lipid class
#'
#' @description Show a violin plot of all RSD values of all lipids per lipid class
#'     within all QCpool samples..
#'
#' @param df tibble in tidy format
#' @param lipidclass_ion character vector containing "lipidclass - ion"
#'
#' @return ggplot2 object
#'
#' @importFrom magrittr %>%
#' @importFrom ggplot2 ggplot aes geom_violin geom_hline scale_colour_manual guides labs theme guide_legend facet_wrap ggtitle coord_flip geom_jitter
#' @importFrom ggCPM theme_cpm cpm_cols
#' @importFrom rlang .data
#'
#'
#' @author Rico Derks
#'
show_rsd_lipidclass_histogram <- function(df, lipidclass_ion) {
  my_colors <- cpm_cols(c("green", "red"))
  names(my_colors) <- c("pos", "neg")

  p <- df %>%
    filter(.data$class_ion %in% lipidclass_ion) %>%
    ggplot(aes(x = .data$LipidClass,
               y = .data$rsd_area)) +
    geom_violin(binwidth = 0.01,
                # alpha = 0.50,
                position = "identity") +
    geom_jitter(aes(colour = .data$polarity)) +
    geom_hline(aes(yintercept = 0.3),
               colour = "red",
               linetype = 2) +
    scale_colour_manual(values = my_colors) +
    guides(colour = guide_legend(title = "Polarity")) +
    labs(y = "Relative standard deviation",
         x = "Lipid class") +
    ggtitle("RSD per lipidclass") +
    # facet_wrap(~ .data$LipidClass,
    #            scales = "free_y",
    #            ncol = 4) +
    coord_flip() +
    theme_cpm() +
    theme(legend.position = "bottom")

  return(p)
}
