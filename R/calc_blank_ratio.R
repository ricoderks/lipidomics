#' @title Calculate the sample to average blank ratio
#'
#' @description Calculate for each lipid the ratio between the sample and the
#'     average of all blanks.
#'
#' @param lipid_data tibble in tidy format
#' @param ratio numeric(1) cutoff ratio of sample/ blank
#'
#' @return Returns a tibble in tidy with sample / blank ratio for each lipid
#'     species.
#'
#' @importFrom dplyr filter group_by summarise left_join mutate ungroup
#' @importFrom rlang .data
#'
#' @author Rico Derks
#'
calc_blank_ratio <- function(lipid_data,
                             ratio = 5) {
  # get the blank data
  blank_df <- lipid_data |>
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
  lipid_data$blankRatio <- lipid_data$area / lipid_data$blankArea

  # calculate the amount of lipid species per sample type above the sample /
  # blank ratio
  lipid_data <- lipid_data |>
    dplyr::group_by(.data$my_id, .data$sample_type) |>
    dplyr::mutate(blank_threshold = mean(.data$blankRatio > ratio, na.rm = TRUE)) |>
    dplyr::ungroup()


  return(lipid_data)
}
