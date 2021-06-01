#' @title Create bubble plot server part
#'
#' @description Create the server part for the bubble plot
#'
#' @param id input id
#' @param data data which is used to make the plot.
#' @param pattern regular expression pattern to select the correct lipid classes.
#' @param lipid_data the wide data frame.
#'
#' @return a part of the server
#'
#' @importFrom shiny moduleServer renderPlot reactive observe observeEvent nearPoints
#' @importFrom magrittr %>%
#' @importFrom dplyr filter select
#' @importFrom rlang .data
#' @importFrom ggplot2 ggplot aes geom_point scale_size geom_line geom_text facet_grid labs guides coord_cartesian
#' @importFrom ggCPM theme_cpm
#'
#' @author Rico Derks
#'
bubblePlotServer <- function(id, data, pattern, lipid_data) {
  moduleServer(
    id = id,
    module = function(input, output, session) {
      ranges <- reactiveValues(x = NULL,
                               y = NULL)

      # zoom out
      observeEvent(input$bubble_dbl, {
        brush <- input$bubble_brush

        if(!is.null(brush)) {
          ranges$x <- c(brush$xmin, brush$xmax)
          ranges$y <- c(brush$ymin, brush$ymax)
        } else {
          ranges$x <- NULL
          ranges$y <- NULL
        }
      })

      # check for zooming
      observe({
        brush <- input$bubble_brush

        if(!is.null(brush)) {
          ranges$x <- c(brush$xmin, brush$xmax)
          ranges$y <- c(brush$ymin, brush$ymax)
        }
      })

      # bubble plot

      output$bubble <- renderPlot({
        data() %>%
          filter(grepl(x = .data$sample_name,
                       pattern = "[qQ][cC]pool_004"),
                 grepl(x = .data$LipidClass,
                       pattern = pattern)) %>%
          ggplot(aes(x = .data$AverageRT,
                     y = .data$AverageMZ,
                     color = .data$carbons)) +
          # group = .data$carbon_db)) +
          geom_point(aes(size = .data$DotProduct),
                     alpha = 0.4) +
          scale_size(range = c(1, 10)) +
          geom_line() +
          geom_text(aes(label = .data$carbon_db),
                    size = 3.0,
                    color = "black") +
          facet_grid(.data$LipidClass ~ .data$ion,
                     scales = "free") +
          labs(x = "Retention time [minutes]",
               y = expression(italic("m/z"))) +
          guides(color = FALSE,
                 size = FALSE) +
          coord_cartesian(xlim = ranges$x,
                          ylim = ranges$y) +
          theme_cpm()
      })

      # show the row clicked
      output$info <- renderTable({
        nearPoints(df = lipid_data() %>%
                     select(.data$my_id:.data$polarity),
                   coordinfo = input$bubble_clk,
                   xvar = "AverageRT",
                   yvar = "AverageMZ",
                   threshold = 10)
      })

      output$msms_clicked <- renderPlot({

      })
    }
  )
}
