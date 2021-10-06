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
          # don't show any lipids which have a large RSD and classes which are not discarded
          filter(.data$class_keep == TRUE,
                 .data$rsd_keep == TRUE) %>%
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
        tmp_lipid_data <- lipid_data()

        # if I use this function the zooming stops working
        # p <- bubble_plot(lipid_data = tmp_lipid_data,
        #                  pattern = pattern)

        # get the sample_name of the first qcpool sample
        selected_name <- tmp_lipid_data %>%
          filter(grepl(x = .data$sample_type,
                       pattern = "[qQ][cC][pP][oO][oO][lL]")) %>%
          arrange(.data$sample_name) %>%
          distinct(.data$sample_name) %>%
          slice(1) %>%
          pull(.data$sample_name)

        if(length(selected_name) == 0) {
          # if no QCpool is in the dataset select the first sample
          selected_name <- tmp_lipid_data %>%
            filter(grepl(x = .data$sample_type,
                         pattern = "[sS][aA][mM][pP][lL][eE]")) %>%
            arrange(.data$sample_name) %>%
            distinct(.data$sample_name) %>%
            slice(1) %>%
            pull(.data$sample_name)
        }

        # get the data to be plotted
        plot_data <-  tmp_lipid_data %>%
          filter(.data$sample_name == selected_name,
                 grepl(x = .data$LipidClass,
                       pattern = pattern),
                 # show only lipid class which are not discarded
                 .data$class_keep == TRUE,
                 # show only lipids which have a low RSD
                 .data$rsd_keep == TRUE)

        # only make plot if data is available
        if(nrow(plot_data) > 0) {

          p <- plot_data %>%
            ggplot(aes(x = .data$AverageRT,
                       y = .data$AverageMZ,
                       color = .data$carbons)) +
            geom_point(aes(size = .data$DotProduct),
                       alpha = 0.4) +
            # show lipid which already should be discarded as grey
            geom_point(data = plot_data[plot_data$keep == FALSE, ],
                       aes(size = .data$DotProduct),
                       color = "grey",
                       alpha = 1) +
            scale_size(range = c(1, 10),
                       limits = c(0, 100)) +
            geom_line() +
            geom_text(aes(label = .data$carbon_db),
                      size = 3.0,
                      color = "black") +
            facet_grid(.data$LipidClass ~ .data$ion,
                       scales = "free") +
            labs(x = "Retention time [minutes]",
                 y = expression(italic("m/z"))) +
            guides(color = "none",
                   size = "none") +
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
          fluidRow(
            column(width = 12,
                   htmlOutput(outputId = session$ns("info"))
            )
          )
        )
      })

      # show the row clicked
      output$info <- renderText({
        # adding selected_data$data here, causes that nothing happens
        req(input$bubble_clk,
            lipid_data_wide)

        # get the data of the selected point
        selected_data$data <- nearPoints(df = lipid_data_wide(),
                                         coordinfo = input$bubble_clk,
                                         xvar = "AverageRT",
                                         yvar = "AverageMZ",
                                         threshold = 10)

        if(nrow(selected_data$data) > 0) {
          # selected_data$data %>%
          #   select(.data$my_id, .data$AverageRT, .data$AverageMZ, .data$ion, .data$ShortLipidName, .data$LongLipidName, .data$LipidClass, .data$DotProduct, .data$RevDotProduct)
          HTML("<table style=\"width:100%\">",
               "<tr><td>ID :</td><td>", selected_data$data$my_id, "</td></tr>",
               "<tr><td>Average RT :</td><td>", selected_data$data$AverageRT, "</td></tr>",
               "<tr><td>Average <i>m/z</i> :</td><td>", selected_data$data$AverageMZ, "</td></tr>",
               "<tr><td>Ion :</td><td>", as.character(selected_data$data$ion), "</td></tr>",
               "<tr><td>Short lipid name :</td><td>", selected_data$data$ShortLipidName, "</td></tr>",
               "<tr><td>Long lipid name :</td><td>", selected_data$data$LongLipidName, "</td></tr>",
               "<tr><td>Lipid class :</td><td>", as.character(selected_data$data$LipidClass), "</td></tr>",
               "<tr><td>Dot product :</td><td>", selected_data$data$DotProduct, "</td></tr>",
               "<tr><td>Reverse dot product :</td><td>", selected_data$data$RevDotProduct, "</td></tr>",
               "</table>")
        } else {
          HTML("<b>More then one point selected!!</b>")
        }
      })

      # show the selection for the reasons
      output$reason_ui <- renderUI({
        req(selected_data$data)

        if(nrow(selected_data$data) == 1) {
          # get the current lipid status
          lipid_status <- lipid_data_wide() %>%
            filter(.data$my_id == selected_data$data$my_id) %>%
            pull(comment)

          # keep doesn't show up in the comments this is NA_character_
          if(is.na(lipid_status) |
             lipid_status == "") {
            lipid_status <- "keep"
          }
        }

        if(nrow(selected_data$data) == 1) {
          tagList(
            fluidRow(
              column(width = 12,
                     selectInput(inputId = session$ns("select_reason"),
                                 label = "Keep :",
                                 choices = c("Keep" = "keep",
                                             "No convincing match" = "no_match",
                                             "Incorrect ret. time" = "wrong_rt",
                                             "High background" = "high_bg"),
                                 selected = lipid_status)
              )
            )
          )
        } else {
          NULL
        }
      })

      observeEvent(input$select_reason, {
        req(selected_data$data,
            lipid_data_wide)

        if(length(selected_data$data$my_id) > 0) {
          toReturn$filter_data <- lipid_data_wide() %>%
            filter(.data$my_id == selected_data$data$my_id) %>%
            select(.data$my_id, .data$keep, .data$comment) %>%
            mutate(keep = if_else(input$select_reason == "keep",
                                  TRUE,
                                  FALSE),
                   comment = input$select_reason)
        }
      },
      ignoreInit = TRUE) # doesn't seem to work

      # output$msms_cutoff_ui <- renderUI({
      #   req(selected_data$data)
      #
      #   tagList(
      #     if(nrow(selected_data$data) == 1) {
      #       sliderInput(inputId = session$ns("msms_cutoff"),
      #                   label = "Annotation cutoff [%]:",
      #                   value = 5,
      #                   min = 0,
      #                   max = 100,
      #                   width = "100%")
      #     } else {
      #       NULL
      #     }
      #   )
      # })

      # output$msms_clicked <- renderPlot({
      #   req(selected_data$data,
      #       input$msms_cutoff)
      #
      #   if(nrow(selected_data$data) == 1) {
      #     msms_data <- selected_data$data %>%
      #       select(.data$MSMSspectrum) %>%
      #       transmute(data_pair = str_split(string = .data$MSMSspectrum,
      #                                       pattern = " ")) %>%
      #       unnest(.data$data_pair) %>%
      #       separate(.data$data_pair,
      #                into = c("mz", "int"),
      #                sep = ":",
      #                convert = TRUE)
      #
      #     msms_data %>%
      #       mutate(rel_int = (.data$int / max(.data$int)) * 100,
      #              show_mz = if_else(.data$rel_int > input$msms_cutoff,
      #                                as.character(.data$mz),
      #                                NA_character_)) %>%
      #       ggplot() +
      #       geom_linerange(aes(x = .data$mz,
      #                          ymin = 0,
      #                          ymax = .data$rel_int)) +
      #       geom_text_repel(aes(x = .data$mz,
      #                           y = .data$rel_int,
      #                           label = .data$show_mz)) +
      #       # scale_y_continuous(labels = scales::scientific) +
      #       labs(x = expression(italic("m/z")),
      #            y = "Relative intensity [%]",
      #            title = "MSMS spectrum acquired") +
      #       theme_minimal()
      #   } else {
      #     return(NULL)
      #   }
      #
      # })

      return(reactive({toReturn}))
    }
  )
}
