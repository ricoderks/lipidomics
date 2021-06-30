#' @title Merge the meta data with the lipid data
#'
#' @description Merge the meta data with the lipid data.
#'
#' @param lipid_data data frame containing the lipid data (wide format).
#' @param meta_data data frame containing the meta data.
#' @param by column name (character) of the the column to use to merge the meta
#'     data with the lipid data
#'
#' @details If there is not meta data the lipid data will be converted to tidy format.
#'
#' @return Returns a merged data frame in tidy format
#'
#' @importFrom dplyr mutate left_join if_else
#' @importFrom tidyr pivot_longer
#' @importFrom stringr str_extract str_replace
#' @importFrom magrittr %>%
#' @importFrom rlang .data
#'
#' @author Rico Derks
#'
merge_data <- function(lipid_data, meta_data = NULL, by = NULL) {

  # make the lipid data long and add some extra columns, same as in tidy_lipids
  lipid_data_long <- lipid_data %>%
    mutate(sample_type = factor(tolower(str_extract(string = .data$sample_name,
                                                    pattern = "([bB]lank|[qQ][cC]pool|[sS]ample)")))) %>%
    # join the meta data
    left_join(y = meta_data,
              by = c("sample_name" = by),
              suffix = c("", ".y"))


  return(lipid_data_long)
}
