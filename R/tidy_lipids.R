#' @title Make the tibble in tidy format
#'
#' @description Filter the tibble to keep only the identified lipids.
#'
#' @param df The tibble.
#'
#' @details After making the tibble in long format also some additional columns ared added.
#'
#' @return Returns a tibble in tidy (long) format
#'
#' @importFrom dplyr mutate
#' @importFrom tidyr pivot_longer
#' @importFrom tidyselect matches
#' @importFrom rlang .data
#' @importFrom stringr str_extract str_replace
#' @importFrom magrittr %>%
#'
#' @author Rico Derks
#'
tidy_lipids <- function(df) {
  # create long table
  df_long <- df %>%
    pivot_longer(cols = matches("^([sS]ample|[qQ][cC]pool|[bB]lank).*"),
                 names_to = "sample_name",
                 values_to = "area") %>%
    mutate(
      # make LipdClass a factor
      LipidClass = factor(.data$LipidClass,
                          levels = sort(unique(.data$LipidClass)),
                          labels = sort(unique(.data$LipidClass))),
      # get the short lipid name
      ShortLipidName = str_extract(string = .data$LipidName,
                                   pattern = "[A-Za-z- 0-9:;/\\(\\)]+"),
      # get the long lipid name
      LongLipidName = str_replace(string = .data$LipidName,
                                  pattern = "([A-Za-z-_ 0-9:;/]*)([|])([A-Za-z-_ 0-9:;]*)",
                                  replacement = "\\3"),
      # correct for empty long lipid names
      LongLipidName = ifelse(.data$LongLipidName == "" | is.na(.data$LongLipidName),
                             .data$ShortLipidName,
                             .data$LongLipidName),
      # a column with number of carbons and double bonds is needed for the bubble plots
      carbons = factor(str_extract(string = .data$ShortLipidName,
                                   pattern = "[0-9]{2}")),
      carbon_db = str_extract(string = .data$ShortLipidName,
                              pattern = "[0-9]{2}:[0-9]{1,2}"),
      sample_type = factor(tolower(str_extract(string = .data$sample_name,
                                               pattern = "([bB]lank|[qQ][cC]pool|[sS]ample)"))),
      class_ion = paste(.data$LipidClass, .data$ion,
                        sep = " - "))

    return(df_long)
}
