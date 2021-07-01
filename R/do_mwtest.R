#' @title Do a Mann-Whitney U test on all lipids
#'
#' @description Do a Mann-Whitney U test on all lipids
#'
#' @param lipid_data tibble in tidy format, already nested
#'
#' @details A Mann-Whitney U test will be done for each lipid. Also the p-value will be corrected
#'     for multiple testing.
#'
#' @return a tibble with the results and the following columns
#'     model_test contains the model information for each lipid (nested)
#'     pvalue contains the uncorrected pvalue
#'     pvalue_cor contains the corrected pvalue
#'     fc the fold change estimate1/estimate2
#'
#' @import tidyselect
#' @importFrom dplyr mutate group_by summarise pull
#' @importFrom tidyr pivot_wider
#' @importFrom magrittr %>%
#' @importFrom rlang .data
#' @importFrom purrr map map_dbl
#' @importFrom broom tidy
#' @importFrom stats p.adjust
#'
#' @author Rico Derks
#'
do_mwtest <- function(lipid_data) {
  results <- lipid_data %>%
    mutate(model_test = map(.x = .data$test_data,
                            .f = ~ broom::tidy(wilcox.test(formula = value ~ my_group_info,
                                                           data = .x))),
           pvalue = map_dbl(.x = .data$model_test,
                            .f = ~ .x$p.value),
           pvalue_adj = p.adjust(.data$pvalue,
                                 method = "BH"),
           p_log10 = -log10(.data$pvalue),
           p_log10_adj = -log10(.data$pvalue_adj))

  return(results)
}

