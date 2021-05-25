#' @title Create bubble plot UI
#'
#' @description Create the UI for the bubble plot
#'
#' @param id id of the UI.
#' @param data data which is used to make the plot.
#' @param pattern regular expression pattern to select the correct lipid classes.
#'
#' @return a part of the UI
#'
#' @importFrom shiny NS fluidRow column plotOutput
#' @importFrom magrittr %>%
#' @importFrom dplyr filter pull
#' @importFrom rlang .data
#'
#' @author Rico Derks
#'
bubblePlotUI <- function(id, data, pattern) {
  ns <- NS(id)

  num_lipid_class <- data %>%
    filter(grepl(x = .data$sample_name,
                 pattern = "[qQ][cC]pool_004"),
           grepl(x = .data$LipidClass,
                 pattern = pattern)) %>%
    pull(.data$LipidClass) %>%
    unique() %>%
    length()

  # calculate new height for the plot
  new_height <- num_lipid_class * 150 + 25
  fluidRow(
    column(width = 8,
          plotOutput(outputId = ns("bubble"),
                     width = "100%",
                     height = paste0(new_height, "px"))
    )
  )

}
