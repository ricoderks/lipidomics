#' @title Do a t-test on all lipids
#'
#' @description Do a t-test on all lipids.
#'
#' @param lipid_data tibble in tidy format, already nested
#'
#' @details A t-test will be done for each lipid. Also the p-value will be corrected
#'     for multiple testing.
#'
#' @return a tibble with the results and the following columns
#'     model_test contains the model information for each lipid (nested)
#'     pvalue contains the uncorrected pvalue
#'     pvalue_cor contains the corrected pvalue
#'     fc the fold change estimate1/estimate2
#'
#' @import tidyselect
#' @importFrom dplyr select filter mutate left_join case_when distinct across
#' @importFrom tidyr pivot_wider everything
#' @importFrom magrittr %>%
#' @importFrom rlang .data
#' @importFrom purrr map map_dbl
#' @importFrom broom tidy
#' @importFrom stats p.adjust
#'
#' @author Rico Derks
#'
do_ttest <- function(lipid_data) {
  results <- lipid_data %>%
    mutate(model_test = map(.x = .data$test_data,
                            .f = ~ broom::tidy(t.test(formula = area ~ my_group_info,
                                                      data = .x)) %>%
                              # calculate the fold change
                              mutate(fc = estimate1 / estimate2)),
           pvalue = map_dbl(.x = .data$model_test,
                            .f = ~ .x$p.value),
           pvalue_adj = p.adjust(.data$pvalue,
                                 method = "BH"),
           fc = map_dbl(.x = .data$model_test,
                        .f = ~ .x$fc),
           p_log10 = -log10(.data$pvalue),
           p_log10_adj = -log10(.data$pvalue_adj),
           fc_log2 = log2(.data$fc))

  return(results)
}

