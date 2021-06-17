#' @title Do PCA analysis
#'
#' @description Do a quick PCA analysis.
#'
#' @param lipid_data tibble in tidy format
#'
#' @return a list with scores, loadings and explained variance
#'
#' @importFrom dplyr select filter mutate
#' @importFrom tidyr pivot_wider
#' @importFrom magrittr %>%
#' @importFrom rlang .data
#' @importFrom janitor clean_names
#' @importFrom recipes recipe update_role step_normalize step_pca all_predictors prep
#'
#' @author Rico Derks
#'
do_pca <- function(lipid_data) {
  # need to make the data wide
  # samples as rows and lipids as columns
  lipid_data_wide <- lipid_data %>%
    select(.data$sample_name, .data$sample_type, .data$ShortLipidName, .data$area) %>%
    pivot_wider(names_from = .data$ShortLipidName,
                values_from = .data$area) %>%
    # only keep samples for now
    filter(.data$sample_type == "sample") %>%
    clean_names()

  pca_rec <- recipe(~.,
                    data = lipid_data_wide) %>%
    # define id variables
    update_role(.data$sample_name, .data$sample_type,
                new_role = "id") %>%
    step_normalize(all_predictors()) %>%
    step_pca(all_predictors())

  pca_prep <- prep(pca_rec)

  return(pca_prep)
}
