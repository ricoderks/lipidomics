#' @title launches the shiny app
#'
#' @description This starts the app locally.
#'
#' @export launchApp
#'
#' @return Shiny application object.
#'
#' @author Rico Derks
#'
#' @examples
#'  \dontrun{
#'  launchApp()
#'  }
#'
#' @import shiny
#' @importFrom shinyjs useShinyjs
#'
launchApp <- function() {
  useShinyjs()

  shinyApp(ui = shinyAppUI, server = shinyAppServer)
}
