#' @title Keep only the identified lipids
#'
#' @description Filter the tibble to keep only the identified lipids.
#'
#' @param lipid_data The tibble.
#'
#' @return Returns a tibble
#'
#' @importFrom dplyr filter arrange
#' @importFrom magrittr %>%
#' @importFrom rlang .data
#'
#' @author Rico Derks
#'
select_identified <- function(lipid_data) {
    # rename some columns in the data frame for ease of access later on.
  lipid_data <- lipid_data %>%
    filter(.data$LipidName != "Unknown",
           .data$LipidClass != "Others",
           # remove annotated peaks without library result
           !grepl(x = .data$LipidName,
                  pattern = "w/o .*"),
           !grepl(x = .data$LipidName,
                  pattern = "RIKEN"),
           !grepl(x = .data$LipidName,
                  pattern = "(no MS2.*|low score.*)")) %>%
    arrange(.data$LipidClass, .data$LipidName, .data$polarity)%>%
    # add some extra columns
    mutate(
      # make LipdClass a factor
      LipidClass = factor(.data$LipidClass,
                          levels = sort(unique(.data$LipidClass)),
                          labels = sort(unique(.data$LipidClass))),
      # get the short lipid name
      ShortLipidName = str_extract(string = .data$LipidName,
                                   pattern = "[A-Za-z- 0-9:;/\\(\\)]+"),
      # make a copy of the ShortLipidName
      orgShortLipidName = .data$ShortLipidName,
      # get the long lipid name
      LongLipidName = str_replace(string = .data$LipidName,
                                  pattern = "([A-Za-z-_ 0-9:;/]*)([|])([A-Za-z-_ 0-9:;]*)",
                                  replacement = "\\3"),
      # correct for empty long lipid names
      LongLipidName = ifelse(.data$LongLipidName == "" | is.na(.data$LongLipidName),
                             .data$ShortLipidName,
                             .data$LongLipidName),
      class_ion = paste(.data$LipidClass, .data$ion,
                        sep = " - "),
      # general keep
      keep = TRUE,
      comment = "",
      # if rsd is too high
      rsd_keep = TRUE,
      # if quality MSMS is too high
      match_keep = TRUE,
      # if retention time is wrong
      rt_keep = TRUE,
      # if the background is too high
      background_keep = TRUE,
      # if class is discarded
      class_keep = TRUE,
      # clean up some levels which might not be present anymore
      ion = droplevels(.data$ion),
      polarity = droplevels(.data$polarity),
      LipidClass = droplevels(.data$LipidClass))

  return(lipid_data)
}
