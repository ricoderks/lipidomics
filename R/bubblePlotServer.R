#' @title Create bubble plot server part
#'
#' @description Create the server part for the bubble plot
#'
#' @param id input id
#' @param lipid_data data which is used to make the plot.
#' @param pattern regular expression pattern to select the correct lipid classes.
#' @param title is the title use on top of the indentication page
#'
#' @return a part of the server
#'
#' @import shiny
#' @importFrom magrittr %>%
#' @importFrom dplyr filter select transmute if_else slice
#' @importFrom tidyr unnest separate pivot_wider
#' @importFrom stringr str_split
#' @importFrom rlang .data
#' @importFrom ggplot2 ggplot aes geom_point scale_size geom_line geom_text facet_grid labs guides coord_cartesian geom_linerange scale_y_continuous theme_minimal theme element_text
#' @importFrom ggrepel geom_text_repel
#' @importFrom scales scientific
#' @importFrom ggCPM theme_cpm
#'
#' @author Rico Derks
#'
bubblePlotServer <- function(id, lipid_data, pattern, title) {
  moduleServer(
    id = id,
    module = function(input, output, session) {
      ranges <- reactiveValues(x = NULL,
                               y = NULL)

      selected_data <- reactiveValues(data = NULL)

      toReturn <- reactiveValues(filter_data = tibble(my_id = character(),
                                                      keep = logical(),
                                                      comment = character()))

      lipid_data_wide <- reactive({
        data_wide <- lipid_data() %>%
          select(-.data$sample_type) %>%
          pivot_wider(id_cols = .data$my_id:.data$carbon_db,
                      names_from = .data$sample_name,
                      values_from = .data$area)

        return(data_wide)
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
        # get the sample_name of the first qcpool sample
        selected_name <- isolate(lipid_data()) %>%
          filter(.data$sample_type == "qcpool") %>%
          arrange(.data$sample_name) %>%
          distinct(.data$sample_name) %>%
          slice(1) %>%
          pull(.data$sample_name)

        plot_data <-  isolate(lipid_data()) %>%
          filter(.data$sample_name == selected_name,
                 grepl(x = .data$LipidClass,
                       pattern = pattern),
                 !(.data$keep == FALSE & .data$comment == "remove_class"),
                 !(.data$keep == FALSE & .data$comment == "large_rsd"))

        # only make plot if data is available
        if(nrow(plot_data) > 0) {
          p <- plot_data %>%
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
        } else {
          p <- NULL
        }

        return(p)
      })

      # show which datapoint is clicked
      output$info_ui <- renderUI({

        tagList(
          column(width = 9,
                 tableOutput(outputId = session$ns("info")))
        )
      })

      # show the selection for the reasons
      output$reason_ui <- renderUI({
        req(selected_data$data)

        if(nrow(selected_data$data) == 1) {
          # get the current lipid status
          lipid_status <- lipid_data_wide() %>%
            filter(.data$my_id == selected_data$data$my_id) %>%
            pull(comment)

          # keep doesn't showup in the comments this is NA_character_
          if(is.na(lipid_status)) {
            lipid_status <- "keep"
          }
        }
        if(nrow(selected_data$data) == 1) {
          tagList(
            column(width = 3,
                   selectInput(inputId = session$ns("select_reason"),
                               label = "Keep:",
                               choices = c("Keep" = "keep",
                                           "No convincing match" = "no_match",
                                           "Incorrect ret. time" = "wrong_rt",
                                           "Rename" = "rename"),
                               selected = lipid_status)
            )
          )
        } else {
          NULL
        }
      })

      observeEvent(input$select_reason, {
        req(selected_data$data)

        toReturn$filter_data <- lipid_data_wide() %>%
          filter(.data$my_id == selected_data$data$my_id) %>%
          select(.data$my_id, .data$keep, .data$comment) %>%
          mutate(keep = if_else(input$select_reason == "keep" |
                                  input$select_reason == "rename",
                                TRUE,
                                FALSE),
                 comment = if_else(input$select_reason == "keep",
                                   "",
                                   input$select_reason))
      },
      ignoreInit = TRUE) # doesn't seem to work

      # show the row clicked
      output$info <- renderTable({
        selected_data$data <- nearPoints(df = lipid_data_wide(),
                                         coordinfo = input$bubble_clk,
                                         xvar = "AverageRT",
                                         yvar = "AverageMZ",
                                         threshold = 10)

        if(nrow(selected_data$data) > 0) {
          selected_data$data %>%
            select(.data$my_id:.data$polarity, -.data$scale_DotProduct, -.data$scale_RevDotProduct)
        } else {
          return(NULL)
        }
      })

      output$msms_cutoff_ui <- renderUI({
        req(selected_data$data)

        tagList(
          if(nrow(selected_data$data) == 1) {
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
        req(selected_data$data,
            input$msms_cutoff)

        if(nrow(selected_data$data) == 1) {
          msms_data <- selected_data$data %>%
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

      return(reactive({toReturn}))
    }
  )
}
