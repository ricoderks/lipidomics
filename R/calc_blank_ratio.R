#' @title Calculate the sample to average blank ratio
#'
#' @description Calculate for each lipid the ratio between the sample and the
#'     average of all blanks.
#'
#' @param lipid_data tibble in tidy format
#'
#' @return Returns a tibble in tidy with RSD values for each lipid.
#'
#' @importFrom dplyr filter group_by summarise ungroup
#' @importFrom magrittr %>%
#' @importFrom rlang .data
#' @importFrom stats sd
#'
#' @author Rico Derks
#'
calc_blank_ratio <- function(lipid_data) {
  # get the blank data
  blank_df <- lipid_data %>%
    dplyr::filter(grepl(x = .data$sample_type,
                        pattern = "[bB][lL][aA][nN][kK]")) |>
    dplyr::group_by(.data$my_id) |>
    dplyr::summarise(blankArea = mean(.data$area, na.rm = TRUE),
                     .groups = "drop")

  # merge
  lipid_data <- lipid_data |>
    dplyr::left_join(y = blank_df,
                     by = c("my_id" = "my_id"))

  # calculate the ratio
  lipid_data$blankRatio = lipid_data$area / lipid_data$blankArea

  return(lipid_data)
}
