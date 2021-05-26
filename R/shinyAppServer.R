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
                             lipid_data_long = NULL,
                             qc_results = NULL,
                             class_ion = NULL,
                             class_ion_selected = NULL,
                             num_lipid_classes = NULL)

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

    all_data$class_ion_selected <- all_data$class_ion

    # regular expression patterns
    pattern_PL <- "^((Ox)?(Ether)?(L)?(LNA)?(MM)?P[ACEGISM]|HBMP|BMP)"
    pattern_GL <- "^(Ox)?(Ether)?(L)?(SQ)?(A)?[DMT]G"
    pattern_Cer <- "^Cer_"
    pattern_HexCer <- "^A?HexCer"
    pattern_FA <- "^(FA|FAHFA|NAGly|NAGlySer|NAOrn|NAE|CAR)"
    pattern_PSL <- "^(ASM|PE-Cer|PE-Cer\\+O|PI-Cer\\+O|SM|SM\\+O)"
    pattern_SB <- "^(PhytoSph|SL|SL\\+O|DHSph|Sph)"
    pattern_SA <- "^(GM3|SHexCer|SHexCer\\+O)"
    pattern_CL <- "^([DM]L)?CL"
    pattern_ACPIM <- "^Ac[2-4]PIM[12]"
    pattern_STL <- "^((BA|S)Sulfate|BileAcid|AHex[BCS][AIRTS][S]?|(BRS|CAS|C|SIS|STS|DCA|TDCA)E|SHex|Cholesterol|VitaminD)"

    my_col_width <- 3

    tagList(
      # this shows one huge checkboxGroupInput
      # column(width = my_col_width,
      #        checkboxGroupInput(inputId = "select_lipidclass_ion",
      #                           label = "Select lipid class:",
      #                           choices = all_data$class_ion,
      #                           selected = all_data$class_ion),
      #        style = "background-color: #E8E8E8"
      # ),
      p("Select the lipid classes you want to keep."),
      column(width = my_col_width,
             checkboxGroupInput(inputId = "select_PL_class",
                                label = "Phospholipids:",
                                choices = all_data$class_ion[grepl(x = all_data$class_ion, pattern = pattern_PL)],
                                selected = all_data$class_ion[grepl(x = all_data$class_ion, pattern = pattern_PL)])
      ),
      column(width = my_col_width,
             checkboxGroupInput(inputId = "select_Cer_class",
                                label = "Ceramides:",
                                choices = all_data$class_ion[grepl(x = all_data$class_ion, pattern = pattern_Cer)],
                                selected = all_data$class_ion[grepl(x = all_data$class_ion, pattern = pattern_Cer)]),
             checkboxGroupInput(inputId = "select_HexCer_class",
                                label = "Neutral glycosphingolipids:",
                                choices = all_data$class_ion[grepl(x = all_data$class_ion, pattern = pattern_HexCer)],
                                selected = all_data$class_ion[grepl(x = all_data$class_ion, pattern = pattern_HexCer)])
      ),
      column(width = my_col_width,
             checkboxGroupInput(inputId = "select_FA_class",
                                label = "Fatty acyls:",
                                choices = all_data$class_ion[grepl(x = all_data$class_ion, pattern = pattern_FA)],
                                selected = all_data$class_ion[grepl(x = all_data$class_ion, pattern = pattern_FA)]),
             checkboxGroupInput(inputId = "select_PSL_class",
                                label = "Phosphosphingolipids:",
                                choices = all_data$class_ion[grepl(x = all_data$class_ion, pattern = pattern_PSL)],
                                selected = all_data$class_ion[grepl(x = all_data$class_ion, pattern = pattern_PSL)]),
             checkboxGroupInput(inputId = "select_SB_class",
                                label = "Sphingoid bases:",
                                choices = all_data$class_ion[grepl(x = all_data$class_ion, pattern = pattern_SB)],
                                selected = all_data$class_ion[grepl(x = all_data$class_ion, pattern = pattern_SB)]),
             checkboxGroupInput(inputId = "select_SA_class",
                                label = "Acidic glycosphingolipids:",
                                choices = all_data$class_ion[grepl(x = all_data$class_ion, pattern = pattern_SA)],
                                selected = all_data$class_ion[grepl(x = all_data$class_ion, pattern = pattern_SA)]),
             checkboxGroupInput(inputId = "select_GL_class",
                                label = "Glcyerolipids:",
                                choices = all_data$class_ion[grepl(x = all_data$class_ion, pattern = pattern_GL)],
                                selected = all_data$class_ion[grepl(x = all_data$class_ion, pattern = pattern_GL)]),
             checkboxGroupInput(inputId = "select_CL_class",
                                label = "Cardiolipins:",
                                choices = all_data$class_ion[grepl(x = all_data$class_ion, pattern = pattern_CL)],
                                selected = all_data$class_ion[grepl(x = all_data$class_ion, pattern = pattern_CL)]),
             checkboxGroupInput(inputId = "select_ACPIM_class",
                                label = "Glycerophosphoinositolglycans:",
                                choices = all_data$class_ion[grepl(x = all_data$class_ion, pattern = pattern_ACPIM)],
                                selected = all_data$class_ion[grepl(x = all_data$class_ion, pattern = pattern_ACPIM)])
      ),
      column(width = my_col_width,
             checkboxGroupInput(inputId = "select_STL_class",
                                label = "Sterol lipids:",
                                choices = all_data$class_ion[grepl(x = all_data$class_ion, pattern = pattern_STL)],
                                selected = all_data$class_ion[grepl(x = all_data$class_ion, pattern = pattern_STL)])
      )
    )
  })

  # for debugging: check which lipid classes / ions are selected
  # output$lipid_classes <- renderText({
  #   req(all_data$class_ion_selected)
  #
  #   all_data$class_ion_selected
  # })

  #### Calculate the RSD values of the QCpool ####
  # show the histogram of all lipids
  output$rsd_all <- renderPlot({
    req(all_data$lipid_data_long)

    # show histogram
    show_rsd_histogram(df = all_data$qc_results)
  })

  # create histogram of all lipids per lipid class
  output$rsd_lipid_classes <- renderPlot({
    req(all_data$qc_results,
        all_data$class_ion_selected)

    # show histogram
    show_rsd_lipidclass_violin(df = all_data$qc_results,
                               lipidclass_ion = all_data$class_ion_selected)
  })

  # create the output UI
  output$rsd_lipidclass_ui <- renderUI({
    req(all_data$qc_results,
        all_data$num_lipid_classes)

    # calculate the new height for the violin plot
    new_height <- ceiling(all_data$num_lipid_classes * 25)

    tagList(
      plotOutput(outputId = "rsd_lipid_classes",
                 width = "50%",
                 height = paste0(new_height, "px"))
    )
  })

  output$create_corplot <- renderPlot({
    req(all_data$lipid_data)

    cor_heatmap(df = all_data$lipid_data)
  })

  # create UI for correlation plot
  output$corplot <- renderUI({
    req(all_data$lipid_data_long,
        all_data$num_lipid_classes)

    # get the number of samples
    num_samples <- all_data$lipid_data_long %>%
      pull(.data$sample_name) %>%
      unique() %>%
      length()

    # calculate the new height for the correlation plot
    new_height <- ceiling(num_samples * 15 + 25)

    tagList(
      plotOutput(outputId = "create_corplot",
                 width = "50%",
                 height = paste0(new_height, "px"))
    )
  })

  output$show_qc_table <- renderTable({
    req(all_data$qc_results,
        all_data$class_ion_selected)

    all_data$qc_results %>%
      filter(.data$class_ion %in% all_data$class_ion_selected)
  })

  #### identification part ####
  # filter the identification data
  observeEvent({
    input$select_PL_class
    input$select_GL_class
    input$select_Cer_class
    input$select_HexCer_class
    input$select_FA_class
    input$select_PSL_class
    input$select_SB_class
    input$select_CL_class
    input$select_ACPIM_class
    input$select_SA_class
    input$select_STL_class
  }, {
    req(all_data$lipid_data_long)
    # get all the selected classes
    all_data$class_ion_selected <- c(input$select_PL_class,
                                     input$select_GL_class,
                                     input$select_Cer_class,
                                     input$select_HexCer_class,
                                     input$select_FA_class,
                                     input$select_PSL_class,
                                     input$select_SB_class,
                                     input$select_CL_class,
                                     input$select_ACPIM_class,
                                     input$select_SA_class,
                                     input$select_STL_class)
    # how many lipid classes are selected
    all_data$num_lipid_classes <- length(unique(sapply(all_data$class_ion_selected, function(x) {
      unlist(strsplit(x = x,
                      split = " - "))[1]
    })))

    # filter the data
    all_data$lipid_data_filter <- all_data$lipid_data_long %>%
      filter(.data$class_ion %in% all_data$class_ion_selected)
  })

  # Ether phospholipids
  output$EPL_UI <- renderUI({
    req(all_data$lipid_data_filter,
        all_data$lipid_data)

    bubblePlotServer(id = "EPL",
                     data = reactive(all_data$lipid_data_filter),
                     pattern = "^EtherL?P[ACEGIS]$",
                     lipid_data = reactive(all_data$lipid_data))

    bubblePlotUI(id = "EPL",
                 data = all_data$lipid_data_filter,
                 pattern = "^EtherL?P[ACEGIS]$")
  })

  # Ether glycerolipids
  output$EGL_UI <- renderUI({
    req(all_data$lipid_data_filter,
        all_data$lipid_data)

    bubblePlotServer(id = "EGL",
                     data = reactive(all_data$lipid_data_filter),
                     pattern = "^Ether[MDT]G$",
                     lipid_data = reactive(all_data$lipid_data))

      bubblePlotUI(id = "EGL",
                 data = all_data$lipid_data_filter,
                 pattern = "^Ether[MDT]G$")
  })

  # glycerolipids
  output$GL_UI <- renderUI({
    req(all_data$lipid_data_filter,
        all_data$lipid_data)

    bubblePlotServer(id = "GL",
                     data = reactive(all_data$lipid_data_filter),
                     pattern = "^[MDT]G$",
                     lipid_data = reactive(all_data$lipid_data))

    bubblePlotUI(id = "GL",
                 data = all_data$lipid_data_filter,
                 pattern = "^[MDT]G$")
  })

  # Lysophospholipids
  output$LPL_UI <- renderUI({
    req(all_data$lipid_data_filter,
        all_data$lipid_data)

    bubblePlotServer(id = "LPL",
                     data = reactive(all_data$lipid_data_filter),
                     pattern = "^LP[ACEGIS]$",
                     lipid_data = reactive(all_data$lipid_data))

    bubblePlotUI(id = "LPL",
                 data = all_data$lipid_data_filter,
                 pattern = "^LP[ACEGIS]$")
  })

  # oxidized phospholipids
  output$OPL_UI <- renderUI({
    req(all_data$lipid_data_filter,
        all_data$lipid_data)

    bubblePlotServer(id = "OPL",
                     data = reactive(all_data$lipid_data_filter),
                     pattern = "^OxP[ACEGIS]$",
                     lipid_data = reactive(all_data$lipid_data))

    bubblePlotUI(id = "OPL",
                 data = all_data$lipid_data_filter,
                 pattern = "^OxP[ACEGIS]$")
  })

  # phospholipids
  output$PL_UI <- renderUI({
    req(all_data$lipid_data_filter,
        all_data$lipid_data)

    bubblePlotServer(id = "PL",
                     data = reactive(all_data$lipid_data_filter),
                     pattern = "^P[ACEGIS]$",
                     lipid_data = reactive(all_data$lipid_data))

    bubblePlotUI(id = "PL",
                 data = all_data$lipid_data_filter,
                 pattern = "^P[ACEGIS]$")
  })

  #### About / Help  section ####
  output$about_session <- renderPrint({
    session_info()
  })
}
