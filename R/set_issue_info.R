#' @title Merge the meta data with the lipid data
#'
#' @description Merge the meta data with the lipid data.
#'
#' @param lipid_data data frame containing the lipid data.
#' @param info data frame containing the issue info
#'
#' @details If there is not meta data the lipid data will be converted to tidy format.
#'
#' @return Returns the same data frame as lipid_data, but with the info added
#'
#' @author Rico Derks
#'
set_issue_info <- function(lipid_data, info) {
  # if nothing is received don't change anything
  if(nrow(info$filter_data) > 0) {
    for(a in 1:nrow(info$filter_data)) {
      # lipid_data$keep[lipid_data$my_id == info$filter_data$my_id[a]] <- info$filter_data$keep[a]
      lipid_data$comment[lipid_data$my_id == info$filter_data$my_id[a]] <- info$filter_data$comment[a]
      switch(info$filter_data$comment[a],
             "keep" = {
               lipid_data$match_keep[lipid_data$my_id == info$filter_data$my_id[a]] <- TRUE
               lipid_data$rt_keep[lipid_data$my_id == info$filter_data$my_id[a]] <- TRUE
             },
             "no_match" = lipid_data$match_keep[lipid_data$my_id == info$filter_data$my_id[a]] <- FALSE,
             "wrong_rt" = lipid_data$rt_keep[lipid_data$my_id == info$filter_data$my_id[a]] <- FALSE)
    }

    lipid_data <- lipid_data %>%
      mutate(keep = if_else(.data$rsd_keep == TRUE &
                              .data$match_keep == TRUE &
                              .data$rt_keep == TRUE,
                            TRUE,
                            FALSE))
  }

  return(lipid_data)
}
