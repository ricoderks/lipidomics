#' @title Shiny app server function
#'
#' @description This is the server function to run the shiny app.
#'
#' @param input provided by shiny
#' @param output provided by shiny
#' @param session provided by shiny
#'
#' @import shiny
#' @importFrom sessioninfo session_info
#'
#' @importFrom tibble tibble
#' @importFrom dplyr filter mutate select arrange pull
#' @importFrom rlang .data
#' @importFrom purrr map
#' @importFrom magrittr %>%
#' @importFrom tidyr unnest
#' @importFrom utils head
#'
#' @author Rico Derks

# Define server logic required to draw a histogram
shinyAppServer <- function(input, output, session) {
  # increase upload limit
  options(shiny.maxRequestSize = 30 * 1024^2)

  file_info <- reactiveValues(pos_file = NULL,
                              pos_datapath = NULL,
                              neg_file = NULL,
                              neg_datapath = NULL,
                              meta_file = NULL,
                              meta_datapath = NULL)

  all_data <- reactiveValues(lipid_data = NULL,
                             lipid_data_long = NULL,
                             qc_results = NULL,
                             class_ion = NULL)

  #### Read the files ####
  # watch the positive mode file
  observe({
    req(input$res_file_pos,
        input$res_file_neg)

    # initialize the tibble for storing all the data
    results <- tibble(filename = c(input$res_file_pos$name, input$res_file_neg$name),
                      datapath = c(input$res_file_pos$datapath, input$res_file_neg$datapath),
                      polarity = c("pos", "neg"))

    # read the data
    results <- results %>%
      filter(.data$datapath != "") %>%
      mutate(raw_data = map(.x = .data$datapath,
                            .f = ~ read_msdial(filename = .x)))

    # make a single dataframe
    results <- results %>%
      select(.data$polarity, .data$raw_data) %>%
      unnest(c(.data$polarity, .data$raw_data))

    # cleanup some column names
    results <- clean_up(df = results)

    # keep only the identified lipids and sort by lipidclass, lipid
    results <- select_identified(results) %>%
      arrange(.data$LipidClass, .data$LipidName, .data$polarity)

    all_data$lipid_data <- results
  })

  # show the raw data
  output$lipid_data_table <- renderTable({
    req(all_data$lipid_data)

    all_data$lipid_data %>%
      head(20)
  })

  # make the lipid data in long format and calculate the RSD values
  observe({
    req(all_data$lipid_data)

    # make the data long
    all_data$lipid_data_long <- tidy_lipids(df = all_data$lipid_data)

    # calculate the RSD values
    all_data$qc_results <- calc_rsd(df = all_data$lipid_data_long)
  })

  #### Select lipid classes ####
  # get the lipid classes
  output$select_lipid_classes <- renderUI({
    req(all_data$lipid_data_long)

    # get all lipid classes with their respective ion
    all_data$class_ion <- all_data$lipid_data_long %>%
      pull(.data$class_ion) %>%
      unique()

    tagList(
      checkboxGroupInput(inputId = "select_lipidclass_ion",
                         label = "Select lipid class:",
                         choices = all_data$class_ion,
                         # selected = c("PC - [M+H]+", "PE - [M+H]+"))
                         selected = all_data$class_ion)
    )
  })

  output$lipid_classes <- renderText({
    req(input$select_lipidclass_ion)

    input$select_lipidclass_ion
  })

  #### Calculate the RSD values of the QCpool ####
  # show the histogram of all lipids
  output$rsd_all <- renderPlot({
    req(all_data$lipid_data_long)

    # show histogram
    show_rsd_histogram(df = all_data$qc_results)
  })

  # show histogram of all lipids per lipid class
  output$rsd_lipid_classes <- renderPlot({
    req(all_data$qc_results,
        input$select_lipidclass_ion)

    class_ion <- input$select_lipidclass_ion

    # show histogram
    show_rsd_lipidclass_histogram(df = all_data$qc_results,
                                  lipidclass_ion = class_ion)
  })

  output$rsd_lipidclass_ui <- renderUI({
    req(all_data$qc_results,
        input$select_lipidclass_ion)

    class_ion <- input$select_lipidclass_ion
    # how many classes are selected
    lipid_class <- unique(sapply(class_ion, function(x) {
      unlist(strsplit(x = x,
               split = " - "))[1]
    }))

    # calculate the new height for the faceting plot
    new_height <- ceiling(length(lipid_class) / 4) * 300

    tagList(
      plotOutput(outputId = "rsd_lipid_classes",
                 width = "75%",
                 height = paste0(new_height, "px"))
    )
  })

  output$show_qc_table <- renderTable({
    req(all_data$qc_results,
        input$select_lipidclass_ion)

    all_data$qc_results %>%
      filter(.data$class_ion %in% input$select_lipidclass_ion)
  })

  #### About / Help  section ####
  output$about_session <- renderPrint({
    session_info()
  })
}
