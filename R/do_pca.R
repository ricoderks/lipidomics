#' @title Do PCA analysis
#'
#' @description Do a quick PCA analysis.
#'
#' @param lipid_data tibble in tidy format
#' @param observations which observations to show
#' @param normalization what normalization to use, none (raw data) or total area normalization
#' @param num_pc number of prinicpal components to calculate (default is 5)
#' @param scaling how to scale the data
#' @param transformation what transformation to use
#'
#' @return a list with scores, loadings and explained variance
#'
#' @import tidyselect
#' @importFrom dplyr select filter mutate left_join case_when distinct across
#' @importFrom tidyr pivot_wider everything
#' @importFrom magrittr %>%
#' @importFrom rlang .data
#' @importFrom janitor clean_names make_clean_names
#' @importFrom recipes recipe update_role step_normalize step_pca all_predictors prep juice tidy step_center step_scale step_log
#'
#' @author Rico Derks
#'
do_pca <- function(lipid_data, observations = c("all", "samples"), normalization = c("raw", "tot_area"), num_pc = 5,
                   scaling = c("none", "uv", "pareto"), transformation = c("none", "log10")) {
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
    filter(.data$keep == TRUE) %>%
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

  # transformation
  switch(transformation,
         "none" = lipid_data_wide <- lipid_data_wide,
         "log10" = lipid_data_wide <- lipid_data_wide %>%
           mutate(across(where(is.numeric), ~ log10(.x + 1))) # the +1 is to avoid problems with 0's
  )

  # center
  lipid_data_wide <- lipid_data_wide %>%
    mutate(across(where(is.numeric), ~ .x - mean(.x)))

  # scale
  switch(scaling,
         "none" = lipid_data_wide <- lipid_data_wide,
         "uv" = lipid_data_wide <- lipid_data_wide %>%
           mutate(across(where(is.numeric), ~ .x / sd(.x))),
         "pareto" = lipid_data_wide <- lipid_data_wide %>%
           mutate(across(where(is.numeric), ~ .x / sqrt(sd(.x))))
  )

  # set up the recipe, preprocessing etc.
  pca_rec <- recipe(~.,
                    data = lipid_data_wide) %>%
    # define id variables
    update_role(.data$sample_name, .data$sample_type,
                new_role = "id") %>%
    step_pca(all_predictors(),
             num_comp = num_pc)

  pca_prep <- prep(pca_rec)

  # get the explained variance
  # get the standard deviation for each prinicipal component
  sdev <- pca_prep$steps[[1]]$res$sdev
  # calculate the cumulate explained variance
  pca_data$explained_var <- tibble(component = factor(paste0("PC", 1:length(sdev)),
                                                      labels = paste0("PC", 1:length(sdev)),
                                                      levels = paste0("PC", 1:length(sdev))),
                                   exp_var = sdev^2 / sum(sdev^2) * 100,
                                   cum_exp_var = cumsum(sdev^2 / sum(sdev^2)) * 100)

  # get the loadings and add some extra columns
  pca_data$loadings <- tidy(pca_prep, 1) %>%
    pivot_wider(id_cols = -.data$id,
                names_from = .data$component,
                values_from = .data$value) %>%
    left_join(y = lipid_data %>%
                select(.data$ShortLipidName, .data$LongLipidName, .data$LipidClass) %>%
                distinct(.data$ShortLipidName, .keep_all = TRUE) %>%
                mutate(clean_lipid_names = make_clean_names(.data$ShortLipidName)),
              by = c("terms" = "clean_lipid_names"))

  # get the scores
  pca_data$scores <- juice(pca_prep, everything())

  return(pca_data)
}

# this is needed for now, because where is not exported, see issue 201 github r-lib/tidyselect
utils::globalVariables("where")
