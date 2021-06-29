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
  for(a in 1:nrow(info$filter_data)) {
    lipid_data$keep[lipid_data$my_id == info$filter_data$my_id[a]] <- info$filter_data$keep[a]
    lipid_data$comment[lipid_data$my_id == info$filter_data$my_id[a]] <- info$filter_data$comment[a]
  }

  return(lipid_data)
}
