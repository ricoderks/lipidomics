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

  lipid_data$keep[lipid_data$my_id == info$filter_data$my_id] <- info$filter_data$keep
  lipid_data$comment[lipid_data$my_id == info$filter_data$my_id] <- info$filter_data$comment
  # lipid_data$ShortLipidName[lipid_data$my_id == info$my_id] <- rename_lipid(lipid_data = all_data$clean_data,
  #                                                                           rename_info = info)

  return(lipid_data)
}
