#' @title Keep only the identified lipids
#'
#' @description Filter the tibble to keep only the identified lipids.
#'
#' @param df The tibble.
#'
#' @return Returns a tibble
#'
#' @importFrom dplyr filter
#' @importFrom magrittr %>%
#' @importFrom rlang .data
#'
#' @author Rico Derks
#'
select_identified <- function(df) {
    # rename some columns in the data frame for ease of access later on.
  df <- df %>%
    filter(.data$LipidName != "Unknown",
           .data$LipidClass != "Others",
           # remove annotated peaks without library result
           !grepl(x = .data$LipidName,
                  pattern = "w/o *"),
           !grepl(x = .data$LipidName,
                  pattern = "RIKEN"))

  return(df)
}
