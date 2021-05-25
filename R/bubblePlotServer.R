#' @title Create bubble plot server part
#'
#' @description Create the server part for the bubble plot
#'
#' @param id id of the server part
#' @param data the data for the server part
#'
#' @return a part of the server
#'
#' @importFrom shiny moduleServer renderPlot
#' @importFrom magrittr %>%
#' @importFrom dplyr filter
#' @importFrom rlang .data
#' @importFrom ggplot2 ggplot aes geom_point scale_size geom_line geom_text facet_grid labs guides
#'
#' @author Rico Derks
#'
bubblePlotServer <- function(id, data, pattern) {
  moduleServer(
    id = id,
    module = function(input, output, session) {

      output$bubble <- renderPlot({

        data %>%
          filter(grepl(x = .data$sample_name,
                       pattern = "[qQ][cC]pool_004"),
                 grepl(x = .data$LipidClass,
                       pattern = pattern))%>%
          ggplot(aes(x = .data$AverageRT,
                     y = .data$AverageMZ,
                     color = .data$carbons,
                     group = .data$carbon_db)) +
          ## assuming this name stays the same
          geom_point(aes(size = .data$DotProduct),
                     alpha = 0.4) +
          scale_size(range = c(1, 10)) +
          geom_line() +
          geom_text(aes(label = .data$carbon_db),
                    size = 2.5,
                    color = "black") +
          facet_grid(.data$LipidClass ~ .data$ion,
                     scales = "free") +
          labs(x = "Retention time [minutes]",
               y = expression(italic("m/z"))) +
          guides(color = FALSE, size = FALSE)
      })
    }
  )
}
