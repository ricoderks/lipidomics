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
#'
launchApp <- function() {
  shinyApp(ui = shinyAppUI, server = shinyAppServer)
}
