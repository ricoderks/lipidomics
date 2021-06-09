#' @title Clean up tibble from MS-DIAL
#'
#' @description Clean up the columns and column names of the tibble after
#'     reading the MS-DIAL result files.
#'
#' @param df The tibble.
#'
#' @return Returns a tibble
#'
#' @importFrom dplyr rename mutate select
#' @importFrom tidyselect matches
#' @importFrom rlang .data
#'
#' @author Rico Derks
#'
clean_up <- function(df) {
  # make a single dataframe
  df <- df %>%
    select(.data$polarity, .data$raw_data) %>%
    unnest(c(.data$polarity, .data$raw_data))

  # rename some columns in the data frame for ease of access later on.
  df <- df %>%
    rename(AlignmentID = .data$`Alignment ID`,
           AverageRT = .data$`Average Rt(min)`,
           AverageMZ = .data$`Average Mz`,
           ion = .data$`Adduct type`,
           LipidName = .data$`Metabolite name`,
           LipidClass = .data$Ontology,
           DotProduct = .data$`Dot product`,
           RevDotProduct = .data$`Reverse dot product`,
           TotalScore = .data$`Total score`,
           FragPresence = .data$`Fragment presence %`,
           RefFile = .data$`Spectrum reference file name`,
           MSMSspectrum = .data$`MS/MS spectrum`) %>%
    mutate(scale_DotProduct = .data$DotProduct / 10,
           scale_RevDotProduct = .data$RevDotProduct / 10,
           my_id = paste(.data$polarity, "_", .data$AlignmentID, sep = "")) %>%
    select(.data$my_id, .data$AlignmentID, .data$AverageRT, .data$AverageMZ, .data$ion, .data$LipidName, .data$LipidClass,
           .data$DotProduct, .data$scale_DotProduct, .data$RevDotProduct, .data$scale_RevDotProduct,
           .data$FragPresence, .data$TotalScore, .data$polarity, .data$MSMSspectrum,
           matches("^([qQ][cC]pool|[sS]ample|[bB]lank)_.*[0-9]{3}$"))

  return(df)
}
