#' @title Make the tibble in tidy format
#'
#' @description Filter the tibble to keep only the identified lipids.
#'
#' @param lipid_data The tibble.
#'
#' @details After making the tibble in long format also some additional columns ared added.
#'
#' @return Returns a tibble in tidy (long) format
#'
#' @importFrom dplyr mutate n group_by ungroup arrange if_else
#' @importFrom tidyr pivot_longer
#' @importFrom tidyselect matches
#' @importFrom rlang .data
#' @importFrom stringr str_extract str_replace
#' @importFrom magrittr %>%
#'
#' @author Rico Derks
#'
tidy_lipids <- function(lipid_data) {
  # create long table
  df_long <- lipid_data %>%
    pivot_longer(cols = matches("^([sS]ample|[qQ][cC]pool|[bB]lank).*"),
                 names_to = "sample_name",
                 values_to = "area") %>%
    mutate(
      # a column with number of carbons and double bonds is needed for the bubble plots
      carbons = factor(str_extract(string = .data$ShortLipidName,
                                   pattern = "[0-9]{1,2}")),
      carbon_db = str_extract(string = .data$ShortLipidName,
                              pattern = "[0-9]{1,2}:?[0-9]{0,2}"),
      sample_type = factor(tolower(str_extract(string = .data$sample_name,
                                               pattern = "([bB]lank|[qQ][cC]pool|[sS]ample)"))))

  ### rename duplicate lipids
  df_long <- df_long %>%
    # determine what are the duplicates
    group_by(.data$ShortLipidName, .data$sample_name) %>%
    arrange(.data$AverageRT) %>%
    mutate(count_duplicates = n(),
           append_name = paste0("_", 1:n())) %>%
    ungroup() %>%
    # rename them
    mutate(ShortLipidName = if_else(.data$count_duplicates > 1,
                                    paste0(.data$ShortLipidName, .data$append_name),
                                    .data$ShortLipidName)) %>%
    # sort back
    arrange(.data$LipidClass, .data$ShortLipidName) %>%
    select(-.data$count_duplicates, -.data$append_name)

    return(df_long)
}
