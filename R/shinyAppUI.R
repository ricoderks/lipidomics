#' @title Shiny app server object
#'
#' @description This is the UI function for the shiny app.
#'
#' @import shiny

# create the shiny application user interface
shinyAppUI <- navbarPage(title = "CPM - Lipidomics",
                         # tabPanel Files
                         tabPanel(title = "Files",
                                  fluidPage(
                                    fluidRow(column = 12,
                                             h4("Files")),
                                    fluidRow(column = 12,
                                             fileInput(inputId = "res_file_pos",
                                                       label = "Positive mode:",
                                                       multiple = FALSE,
                                                       accept = c(".txt"),
                                                       width = 400),
                                             fileInput(inputId = "res_file_neg",
                                                       label = "Negative mode:",
                                                       multiple = FALSE,
                                                       accept = c(".txt"),
                                                       width = 400))
                                  )
                         ), # end tabPane Files
                         # tabPanel About
                         tabPanel(title = "About",
                                  fluidPage(
                                    fluidRow(column = 12,
                                             h3("Issues"),
                                             p("If you have any ideas to extend this shiny app please send me an email. If you have any issue please send me an email or go to the ",
                                               a("issue tracker.", href = "https://github.com/ricoderks/lipidomics/issues", target = "_blank"),
                                               "Cheers, Rico")),
                                    fluidRow(column = 12,
                                             h3("Session info"),
                                             verbatimTextOutput("about_session"))
                                  )
                         ) # end tabPanel About
)
