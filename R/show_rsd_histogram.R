#' @title Show the histogram of all RSD values
#'
#' @description Show the histogram of all RSD values of all lipids within
#'     all QCpool samples..
#'
#' @param df tibble in tidy format
#'
#' @return ggplot2 object
#'
#' @importFrom magrittr %>%
#' @importFrom ggplot2 ggplot aes geom_histogram geom_vline scale_fill_manual guides labs theme guide_legend
#' @importFrom ggCPM theme_cpm cpm_cols
#' @importFrom rlang .data
#'
#'
#' @author Rico Derks
#'
show_rsd_histogram <- function(df) {
  my_colors <- cpm_cols(c("green", "red"))
  names(my_colors) <- c("pos", "neg")

  p <- df %>%
    ggplot(aes(.data$rsd_area)) +
    geom_histogram(aes(fill = .data$polarity),
                   binwidth = 0.005,
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
    theme_cpm() +
    theme(legend.position = "bottom")

  return(p)
}
