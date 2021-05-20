#' @title Calculate RSD values of all lipids
#'
#' @description Calculate RSD values of all lipids within all QCpool samples..
#'
#' @param df tibble in tidy format
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
calc_rsd <- function(df) {
  # create long table
  qc_df <- df %>%
    filter(.data$sample_type == "qcpool") %>%
    group_by(.data$my_id, .data$polarity) %>%
    summarise(rsd_area = sd(.data$area, na.rm = TRUE) / mean(.data$area, na.rm = TRUE),
              LipidClass = .data$LipidClass[1],
              ShortLipidName = .data$ShortLipidName[1],
              LongLipidName = .data$LongLipidName[1]) %>%
    ungroup()

  return(qc_df)
}
