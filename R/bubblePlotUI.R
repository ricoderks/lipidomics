#' @title Create bubble plot UI
#'
#' @description Create the UI for the bubble plot
#'
#' @param id id of the UI
#'
#' @return a part of the UI
#'
#' @importFrom shiny NS fluidRow column plotOutput
#'
#' @author Rico Derks
#'
bubblePlotUI <- function(id) {
  ns <- NS(id)

  plotOutput(outputId = ns("bubble"),
             width = "100%")
}
