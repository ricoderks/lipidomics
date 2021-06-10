#' @title Merge the meta data with the lipid data
#'
#' @description Merge the meta data with the lipid data.
#'
#' @param lipid_data data frame containing the lipid data (wide format).
#' @param meta_data data frame containing the meta data.
#' @param by column name (character) of the the column to use to merge the meta
#'     data with the lipid data
#'
#' @details If there is not meta data the lipid data will be converted to tidy format.
#'
#' @return Returns a merged data frame in tidy format
#'
#' @importFrom dplyr mutate left_join if_else
#' @importFrom tidyselect matches
#' @importFrom tidyr pivot_longer
#' @importFrom stringr str_extract str_replace
#' @importFrom magrittr %>%
#' @importFrom rlang .data
#'
#' @author Rico Derks
#'
merge_data <- function(lipid_data, meta_data = NULL, by = NULL) {

  # make the lipid data long and add some extra columns, same as in tidy_lipids
  lipid_data_long <- lipid_data %>%
    pivot_longer(cols = matches("^([sS]ample|[qQ][cC]pool|[bB]lank).*"),
                 names_to = "sample_name",
                 values_to = "area") %>%
    mutate(
      # make LipdClass a factor
      LipidClass = factor(.data$LipidClass,
                          levels = sort(unique(.data$LipidClass)),
                          labels = sort(unique(.data$LipidClass))),
      # get the short lipid name
      ShortLipidName = str_extract(string = .data$LipidName,
                                   pattern = "[A-Za-z- 0-9:;/\\(\\)]+"),
      # make a copy of the short lipid name column
      orgShortLipidName = .data$ShortLipidName,
      # get the long lipid name
      LongLipidName = str_replace(string = .data$LipidName,
                                  pattern = "([A-Za-z-_ 0-9:;/]*)([|])([A-Za-z-_ 0-9:;]*)",
                                  replacement = "\\3"),
      # correct for empty long lipid names
      LongLipidName = ifelse(.data$LongLipidName == "" | is.na(.data$LongLipidName),
                             .data$ShortLipidName,
                             .data$LongLipidName),
      # a column with number of carbons and double bonds is needed for the bubble plots
      carbons = factor(str_extract(string = .data$ShortLipidName,
                                   pattern = "[0-9]{2}")),
      carbon_db = str_extract(string = .data$ShortLipidName,
                              pattern = "[0-9]{2}:[0-9]{1,2}"),
      sample_type = factor(tolower(str_extract(string = .data$sample_name,
                                               pattern = "([bB]lank|[qQ][cC]pool|[sS]ample)"))),
      # some lipids need to be renamed, this does not work, it removes all ShortLipidName
      ShortLipidName = ifelse(.data$comment == "rename",
                              paste0(.data$orgShortLipidName, .data$append_name),
                              .data$orgShortLipidName)
    )

  # if there is no meta data only return the data in long format
  if(!is.null(meta_data)) {
    lipid_data_long <- lipid_data_long %>%
      # join the meta data
      left_join(y = meta_data,
                by = c("sample_name" = by),
                suffix = c("", ".y"))
  }

  # only select a few columns
  lipid_data_long <- lipid_data_long%>%
    select(-c(.data$DotProduct:.data$TotalScore), -.data$MSMSspectrum)

  return(lipid_data_long)
}
