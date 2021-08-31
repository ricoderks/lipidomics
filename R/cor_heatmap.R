#' @title Create correlation heatmap of all samples
#'
#' @description Create a correlation heatmap of all samples..
#'
#' @param lipid_data tibble with all the lipid data
#'
#' @return plotly object
#'
#' @importFrom magrittr %>%
#' @importFrom dplyr select
#' @importFrom tidyselect matches
#' @importFrom stringr str_extract
#' @importFrom rlang .data
#' @importFrom plotly plot_ly colorbar
#' @importFrom stats cor
#' @importFrom grDevices colorRamp
#'
#' @export
#'
#' @author Rico Derks
#'
cor_heatmap <- function(lipid_data) {
  df_m <- lipid_data %>%
    select(matches("([qQ][cC]pool|[sS]ample)"))

  # calculate the correlation
  cormat <- cor(df_m)

  p <- plot_ly(x = colnames(df_m),
              y = colnames(df_m),
              z = cormat,
              type = "heatmap",
              colors = colorRamp(c("blue", "red"))) %>%
    colorbar(limits = c(-1, 1))

  return(p)
}
