#' @title Keep only the identified lipids
#'
#' @description Filter the tibble to keep only the identified lipids.
#'
#' @param lipid_data The tibble.
#'
#' @return Returns a tibble
#'
#' @importFrom dplyr filter arrange
#' @importFrom magrittr %>%
#' @importFrom rlang .data
#'
#' @author Rico Derks
#'
select_identified <- function(lipid_data) {
    # rename some columns in the data frame for ease of access later on.
  lipid_data <- lipid_data %>%
    filter(.data$LipidName != "Unknown",
           .data$LipidClass != "Others",
           # remove annotated peaks without library result
           !grepl(x = .data$LipidName,
                  pattern = "w/o *"),
           !grepl(x = .data$LipidName,
                  pattern = "RIKEN")) %>%
    arrange(.data$LipidClass, .data$LipidName, .data$polarity)

  return(lipid_data)
}
