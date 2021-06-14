#' @title Rename lipids automatically
#'
#' @description Rename a seleceted lipid automatically. Append the lipid name with a number.
#'
#' @param lipid_data data frame containing the lipid data (wide format).
#' @param rename_info data frame containing the information which lipid to rename
#'
#' @details rename_info contains more then only which lipids to rename, ignore the rest.
#'
#' @return Returns a data frame with the lipid renamed.
#'
#' @importFrom dplyr filter pull arrange if_else mutate
#' @importFrom magrittr %>%
#' @importFrom rlang .data
#'
#' @author Rico Derks
#'
rename_lipid <- function(lipid_data, rename_info) {
  if(rename_info$comment == "rename") {
    # get the short lipid name
    short_name <- lipid_data %>%
      filter(.data$my_id == rename_info$my_id) %>%
      pull(.data$orgShortLipidName)

    # lookup how many duplicates there are
    duplicates <- lipid_data %>%
      filter(.data$orgShortLipidName == short_name) %>%
      # sort by retention time
      arrange(.data$AverageRT) %>%
      pull(.data$my_id)

    # create the text to append to the short lipid name
    append_text <- paste0("_", which(duplicates == rename_info$my_id))

    # rename the lipid
    new_lipid_name <- lipid_data %>%
      filter(.data$my_id == rename_info$my_id) %>%
      mutate(ShortLipidName = paste0(.data$orgShortLipidName, append_text)) %>%
      pull(.data$ShortLipidName)
  } else {
    # do nothing, but return the original short lipid name
    new_lipid_name <- lipid_data %>%
      filter(.data$my_id == rename_info$my_id) %>%
      pull(.data$orgShortLipidName)
  }

  # print(new_lipid_name)

  return(new_lipid_name)
}
