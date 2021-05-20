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
#' @importFrom dplyr filter mutate select arrange
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
                             meta_data = NULL)

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


  #### About / Help  section ####
  output$about_session <- renderPrint({
    session_info()
  })
}
