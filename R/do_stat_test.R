#' @title Do a statitical test
#'
#' @description Do a statitical test, t-test or Mann-Whitney U test
#'
#' @param lipid_data tibble in tidy format
#' @param group what column contains the group info
#' @param group1_name is the name of group 1
#' @param group2_name is the name of group 2
#' @param normalization what normalization to use, none (raw data) or total area normalization
#' @param transformation what transformation to use
#' @param test do a t-test or Mann-Whitney U test
#'
#' @return a tibble ready for statistical testing
#'
#' @import tidyselect
#' @importFrom dplyr select filter mutate rename group_by ungroup
#' @importFrom tidyr pivot_wider everything nest
#' @importFrom purrr map_dbl
#' @importFrom magrittr %>%
#' @importFrom rlang .data
#'
#' @author Rico Derks
#'
do_stat_test <- function(lipid_data, group, group1_name, group2_name, normalization = c("raw", "tot_area"), transformation = c("none", "log10"),
                              test) {

  if(group1_name != "none" &
     group2_name != "none" &
     group1_name != group2_name) {
    prep_test_data <- lipid_data %>%
      filter(.data$keep == TRUE) %>%
      rename(my_group_info = !!sym(group)) %>%
      filter(.data$my_group_info == group1_name |
               .data$my_group_info == group2_name) %>%
      select(.data$my_id, .data$ShortLipidName, .data$LipidClass, .data$sample_name, .data$my_group_info, .data$area) %>%
      # total area normalisation
      group_by(.data$sample_name) %>%
      mutate(norm_area = .data$area / sum(.data$area)) %>%
      ungroup() %>%
      # select which normalization to use for PCA
      mutate(value = case_when(
        normalization == "raw" ~ .data$area,
        normalization == "tot_area" ~ .data$norm_area
      )) %>%
      # do transformations and select which transformation to keep
      mutate(value = case_when(
        transformation == "none" ~ .data$value,
        transformation == "log10" ~ log10(.data$value + 1) # the +1 is correct for any zero's
      )) %>%
      # remove the 2 area columns
      select(-.data$area, -.data$norm_area) %>%
      nest(test_data = c(.data$sample_name, .data$my_group_info, .data$value)) %>%
      mutate(fc = map_dbl(.x = .data$test_data,
                          .f = ~ mean(.x$value[.x$my_group_info == group1_name]) / mean(.x$value[.x$my_group_info == group2_name])),
             fc_log2 = log2(.data$fc))

    result <- switch(test,
                     "ttest" = do_ttest(lipid_data = prep_test_data),
                     "mwtest" = do_mwtest(lipid_data = prep_test_data))
  } else {
    result <- NULL
  }

  return(result)
}
