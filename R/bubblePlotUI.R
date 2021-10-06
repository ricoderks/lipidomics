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
#' @importFrom dplyr filter pull distinct
#' @importFrom rlang .data
#'
#' @author Rico Derks
#'
bubblePlotUI <- function(id, data, pattern) {
  ns <- NS(id)

  if(nrow(data) > 0) {
    # get the sample_name of the first qcpool sample
    selected_name <- data %>%
      filter(grepl(x = .data$sample_type,
                   pattern = "[qQ][cC][pP][oO][oO][lL]")) %>%
      arrange(.data$sample_name) %>%
      distinct(.data$sample_name) %>%
      slice(1) %>%
      pull(.data$sample_name)

    if(length(selected_name) == 0) {
      # if no QCpool is in the dataset select the first sample
      selected_name <- data %>%
        filter(grepl(x = .data$sample_type,
                     pattern = "[sS][aA][mM][pP][lL][eE]")) %>%
        arrange(.data$sample_name) %>%
        distinct(.data$sample_name) %>%
        slice(1) %>%
        pull(.data$sample_name)
    }

    num_lipid_class <- data %>%
      filter(.data$sample_name == selected_name,
             grepl(x = .data$LipidClass,
                   pattern = pattern),
             .data$class_keep == TRUE) %>%
      distinct(.data$LipidClass) %>%
      pull(.data$LipidClass) %>%
      length()
  } else {
    num_lipid_class <- 1
  }

  # calculate new height for the plot
  new_height <- num_lipid_class * 175 + 25
  fluidPage(
    # show the selected point
    fluidRow(
      column(width = 12,
             uiOutput(outputId = ns("show_tab_id_ui")))
    ),
    # show table with info about point clicked
    fluidRow(
      # table
      # uiOutput(outputId = ns("info_ui")),
      # # reason selection
      # uiOutput(outputId = ns("reason_ui"))
    ),
    # show the bubble plot
    fluidRow(
      column(width = 8,
             plotOutput(outputId = ns("bubble"),
                        width = "100%",
                        height = paste0(new_height, "px"),
                        brush = brushOpts(
                          id = ns("bubble_brush"),
                          delayType = "debounce",
                          resetOnNew = TRUE
                        ),
                        dblclick = dblclickOpts(id = ns("bubble_dbl")),
                        click = clickOpts(id = ns("bubble_clk")))
      ),
      column(width = 4,
             # # reason selection
             uiOutput(outputId = ns("reason_ui")),
             # table
             uiOutput(outputId = ns("info_ui"))

      )
    )
  )
}
