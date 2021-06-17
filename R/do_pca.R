#' @title Do PCA analysis
#'
#' @description Do a quick PCA analysis.
#'
#' @param lipid_data tibble in tidy format
#' @param observations which observations to show
#' @param normalization what normalization to use, none (raw data) or total area normalization
#'
#' @return a list with scores, loadings and explained variance
#'
#' @importFrom dplyr select filter mutate left_join case_when
#' @importFrom tidyr pivot_wider everything
#' @importFrom magrittr %>%
#' @importFrom rlang .data
#' @importFrom janitor clean_names
#' @importFrom recipes recipe update_role step_normalize step_pca all_predictors prep juice
#'
#' @author Rico Derks
#'
do_pca <- function(lipid_data, observations = c("all", "samples"), normalization = c("raw", "tot_area")) {
  # initialize result list
  pca_data <- list(scores = NULL,
                   loadings = NULL,
                   explained_var = NULL)

  # select which samples to show
  if(observations == "all") {
    select_obs <- c("sample", "qcpool")
  } else if(observations == "samples") {
    select_obs <- "sample"
  }

  # need to make the data wide
  # samples as rows and lipids as columns
  lipid_data_wide <- lipid_data %>%
    # select which sample type to keep
    filter(.data$sample_type %in% select_obs) %>%
    # total area normalisation
    group_by(.data$sample_name) %>%
    mutate(norm_area = .data$area / sum(.data$area)) %>%
    ungroup() %>%
    # select what to use for PCA
    mutate(value = case_when(
      normalization == "raw" ~ .data$area,
      normalization == "tot_area" ~ .data$norm_area
    )) %>%
    # make wide
    select(.data$sample_name, .data$sample_type, .data$ShortLipidName, .data$value) %>%
    pivot_wider(names_from = .data$ShortLipidName,
                values_from = .data$value) %>%
    clean_names()

  # set up the recipe, preprocessing etc.
  pca_rec <- recipe(~.,
                    data = lipid_data_wide) %>%
    # define id variables
    update_role(.data$sample_name, .data$sample_type,
                new_role = "id") %>%
    # this is centering and scaling
    step_normalize(all_predictors()) %>%
    step_pca(all_predictors())

  pca_prep <- prep(pca_rec)

  pca_data$scores <- juice(pca_prep, everything())

  return(pca_data)
}
