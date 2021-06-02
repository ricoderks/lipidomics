#' @title Create bubble plot server part
#'
#' @description Create the server part for the bubble plot
#'
#' @param id input id
#' @param data data which is used to make the plot.
#' @param pattern regular expression pattern to select the correct lipid classes.
#' @param lipid_data the wide data frame.
#' @param title is the title use on top of the indentication page
#' @param filter_data data frame which will contain the information about which lipid needs to be removed and why
#'
#' @return a part of the server
#'
#' @import shiny
#' @importFrom magrittr %>%
#' @importFrom dplyr filter select transmute if_else
#' @importFrom tidyr unnest separate
#' @importFrom stringr str_split
#' @importFrom rlang .data
#' @importFrom ggplot2 ggplot aes geom_point scale_size geom_line geom_text facet_grid labs guides coord_cartesian geom_linerange scale_y_continuous theme_minimal theme element_text
#' @importFrom ggrepel geom_text_repel
#' @importFrom scales scientific
#' @importFrom ggCPM theme_cpm
#'
#' @author Rico Derks
#'
bubblePlotServer <- function(id, data, pattern, lipid_data, title, filter_data) {
  moduleServer(
    id = id,
    module = function(input, output, session) {
      ranges <- reactiveValues(x = NULL,
                               y = NULL)

      data <- reactiveValues(selected_data = NULL)

      my_filter <- reactiveValues(filter_data = filter_data)

      # # store all lipid data in filter_data
      # data$filter_data <- tibble(my_id = lipid_data()$my_id,
      #                            keep = TRUE,
      #                            comment = NA_character_)

      # this just for debugging -> can be removed
      output$my_debug <- renderTable({

        my_filter$filter_data
      })

      # show which identification tab is selected
      output$show_tab_id_ui <- renderUI({
        tagList(
          h3(title)
        )
      })

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
          theme_cpm() +
          theme(strip.text = element_text(size = 10))
      })

      # show which datapoint is clicked
      output$info_ui <- renderUI({

        tagList(
          column(width = 8,
                 tableOutput(outputId = session$ns("info")))
        )
      })

      # show the selection for the reasons
      output$reason_ui <- renderUI({
        req(data$selected_data)

        tagList(
          column(width = 4,
                 if(nrow(data$selected_data) == 1) {
                   selectInput(inputId = session$ns("select_reason"),
                               label = "Keep:",
                               choices = c("Keep" = "keep",
                                           "No convincing match" = "no_match",
                                           "Incorrect ret. time" = "wrong_rt"),
                               selected = "keep")
                 } else {
                   NULL
                 }
          )
        )
      })

      observeEvent(input$select_reason, {
        req(data$selected_data)

        my_filter$filter_data <- my_filter$filter_data %>%
          mutate(comment = if_else(.data$my_id == data$selected_data$my_id,
                                   input$select_reason,
                                   .data$comment))
      })

      # show the row clicked
      output$info <- renderTable({
        data$selected_data <- nearPoints(df = lipid_data(),
                                         coordinfo = input$bubble_clk,
                                         xvar = "AverageRT",
                                         yvar = "AverageMZ",
                                         threshold = 10)

        if(nrow(data$selected_data) > 0) {
          data$selected_data %>%
            select(.data$my_id:.data$polarity, -.data$scale_DotProduct, -.data$scale_RevDotProduct)
        } else {
          return(NULL)
        }
      })

      output$msms_cutoff_ui <- renderUI({
        req(data$selected_data)

        tagList(
          if(nrow(data$selected_data) == 1) {
            sliderInput(inputId = session$ns("msms_cutoff"),
                        label = "Annotation cutoff [%]:",
                        value = 5,
                        min = 0,
                        max = 100,
                        width = "100%")
          } else {
            NULL
          }
        )
      })

      output$msms_clicked <- renderPlot({
        req(data$selected_data,
            input$msms_cutoff)

        if(nrow(data$selected_data) == 1) {
          msms_data <- data$selected_data %>%
            select(.data$MSMSspectrum) %>%
            transmute(data_pair = str_split(string = .data$MSMSspectrum,
                                            pattern = " ")) %>%
            unnest(.data$data_pair) %>%
            separate(.data$data_pair,
                     into = c("mz", "int"),
                     sep = ":",
                     convert = TRUE)

          msms_data %>%
            mutate(rel_int = (.data$int / max(.data$int)) * 100,
                   show_mz = if_else(.data$rel_int > input$msms_cutoff,
                                     as.character(.data$mz),
                                     NA_character_)) %>%
            ggplot() +
            geom_linerange(aes(x = .data$mz,
                               ymin = 0,
                               ymax = .data$rel_int)) +
            geom_text_repel(aes(x = .data$mz,
                                y = .data$rel_int,
                                label = .data$show_mz)) +
            # scale_y_continuous(labels = scales::scientific) +
            labs(x = expression(italic("m/z")),
                 y = "Relative intensity [%]",
                 title = "MSMS spectrum acquired") +
            theme_minimal()
        } else {
          return(NULL)
        }

      })
    }
  )
}
