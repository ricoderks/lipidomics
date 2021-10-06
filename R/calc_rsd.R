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
  # check if there are QCpool present
  qc_df <- lipid_data %>%
    filter(grepl(x = .data$sample_type,
                 pattern = "[qQ][cC][pP][oO][oO][lL]"))
  if(nrow(qc_df) > 0) {
    # QCpool present so calculate RSD values
    qc_df <- qc_df %>%
      group_by(.data$my_id, .data$polarity) %>%
      summarise(rsd_area = sd(.data$area, na.rm = TRUE) / mean(.data$area, na.rm = TRUE),
                LipidClass = .data$LipidClass[1],
                ShortLipidName = .data$ShortLipidName[1],
                LongLipidName = .data$LongLipidName[1],
                class_ion = .data$class_ion[1]) %>%
      ungroup()
  } else {
    # no QCpool, set RSD values to 0
    qc_df <- lipid_data %>%
      group_by(.data$my_id, .data$polarity) %>%
      summarise(rsd_area = 0,
                LipidClass = .data$LipidClass[1],
                ShortLipidName = .data$ShortLipidName[1],
                LongLipidName = .data$LongLipidName[1],
                class_ion = .data$class_ion[1]) %>%
      ungroup()
  }

  # create long table



  return(qc_df)
}
