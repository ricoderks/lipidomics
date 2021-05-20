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
#' @importFrom readr read_delim cols col_double col_character col_integer
#' @importFrom tibble tibble
#' @importFrom dplyr filter mutate select
#' @importFrom tidyr unnest
#' @importFrom rlang .data
#' @importFrom purrr map
#' @importFrom magrittr %>%
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

  all_data <- reactiveValues(raw_data = NULL)

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
                            .f = ~ read_delim(file = .x,
                                              delim ="\t",
                                              na = c("", "NA", "null"),
                                              col_types = cols(`Alignment ID` = col_integer(),
                                                               `Average Rt(min)` = col_double(),
                                                               `Average Mz` = col_double(),
                                                               `Metabolite name` = col_character(),
                                                               `Adduct type` = col_character(),
                                                               `Post curation result` = col_character(),
                                                               `Fill %` = col_double(),
                                                               `MS/MS assigned` = col_character(),
                                                               `Reference RT` = col_character(),
                                                               `Reference m/z` = col_character(),
                                                               Formula = col_character(),
                                                               Ontology = col_character(),
                                                               INCHIKEY = col_character(),
                                                               SMILES = col_character(),
                                                               `Annotation tag (VS1.0)` = col_character(),
                                                               `RT matched` = col_character(),
                                                               `m/z matched` = col_character(),
                                                               `MS/MS matched` = col_character(),
                                                               Comment = col_character(),
                                                               `Manually modified for quantification` = col_character(),
                                                               `Manually modified for annotation` = col_character(),
                                                               `Isotope tracking parent ID` = col_character(),
                                                               `Isotope tracking weight number` = col_character(),
                                                               `Total score` = col_double(),
                                                               `RT similarity` = col_double(),
                                                               `Dot product` = col_double(),
                                                               `Reverse dot product` = col_double(),
                                                               `Fragment presence %` = col_double(),
                                                               `S/N average` = col_double(),
                                                               `Spectrum reference file name` = col_character(),
                                                               `MS1 isotopic spectrum` = col_character(),
                                                               `MS/MS spectrum` = col_character()),
                                              skip = 4)))
    # make a single dataframe
    results <- results %>%
      select(.data$polarity, .data$raw_data) %>%
      unnest(c(.data$polarity, .data$raw_data))

    all_data$raw_data <- results
  })

  output$temp_table <- renderTable({
    req(all_data$raw_data)

    all_data$raw_data %>%
      head(20)
  })


  #### About / Help  section ####
  output$about_session <- renderPrint({
    session_info()
  })
}
