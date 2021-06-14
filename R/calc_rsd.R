#' @title Calculate RSD values of all lipids
#'
#' @description Calculate RSD values of all lipids within all QCpool samples..
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
calc_rsd <- function(lipid_data) {
  # create long table
  qc_df <- lipid_data %>%
    filter(.data$sample_type == "qcpool") %>%
    group_by(.data$my_id, .data$polarity) %>%
    summarise(rsd_area = sd(.data$area, na.rm = TRUE) / mean(.data$area, na.rm = TRUE),
              LipidClass = .data$LipidClass[1],
              ShortLipidName = .data$ShortLipidName[1],
              LongLipidName = .data$LongLipidName[1],
              class_ion = .data$class_ion[1]) %>%
    ungroup()

  return(qc_df)
}
