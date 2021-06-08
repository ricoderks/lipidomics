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
                             lipid_data_filter = NULL,
                             clean_data = NULL,
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

    # keep only the identified lipids and sort by lipid class, lipid
    results <- select_identified(results) %>%
      arrange(.data$LipidClass, .data$LipidName, .data$polarity)

    # add some extra columns for lipid selection
    all_data$lipid_data <- results %>%
      mutate(keep = TRUE,
             comment = NA_character_,
             class_ion = paste(.data$LipidClass, .data$ion,
                               sep = " - "))
  })

  # show the raw data
  output$lipid_data_table <- renderTable({
    req(all_data$lipid_data)

    all_data$lipid_data %>%
      # remove a few columns
      select(-.data$MSMSspectrum, -.data$scale_DotProduct, -.data$scale_RevDotProduct, -.data$keep, -.data$comment) %>%
      head(20)
  })

  # make the lipid data in long format and calculate the RSD values
  observe({
    req(all_data$lipid_data)

    # make a copy of the original data and work with this
    all_data$clean_data <- all_data$lipid_data

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
    pattern_GL <- "^(Ox|Ether|SQ|EtherS|L|A)?[DMT]G"
    pattern_Cer <- "^Cer[P_]"
    pattern_HexCer <- "^A?Hex[23]?Cer"
    pattern_FA <- "^((Ox)?FA|FAHFA|NAGly|NAGlySer|NAOrn|NAE|CAR)"
    pattern_PSL <- "^(ASM|PE_Cer(\\+O)?|PI_Cer(\\+O)?|SM|SM\\+O)"
    pattern_SB <- "^(PhytoSph|SL|SL\\+O|DHSph|Sph)"
    pattern_SA <- "^(GM3|SHexCer|SHexCer\\+O)"
    pattern_CL <- "^([DM]L)?CL"
    pattern_ACPIM <- "^Ac[2-4]PIM[12]"
    pattern_STL <- "^((BA|S)Sulfate|BileAcid|AHex[BCS][AIRTS][S]?|(BRS|CAS|C|SIS|STS|DCA|TDCA)E|SHex|Cholesterol|VitaminD|ST)"
    pattern_PRL <- "^(VAE|CoQ|VitaminE)"

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
                                label = "Glycerophospholipids:",
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
                                selected = all_data$class_ion[grepl(x = all_data$class_ion, pattern = pattern_CL)])
      ),
      column(width = my_col_width,
             checkboxGroupInput(inputId = "select_STL_class",
                                label = "Sterol lipids:",
                                choices = all_data$class_ion[grepl(x = all_data$class_ion, pattern = pattern_STL)],
                                selected = all_data$class_ion[grepl(x = all_data$class_ion, pattern = pattern_STL)]),
             checkboxGroupInput(inputId = "select_ACPIM_class",
                                label = "Glycerophosphoinositolglycans:",
                                choices = all_data$class_ion[grepl(x = all_data$class_ion, pattern = pattern_ACPIM)],
                                selected = all_data$class_ion[grepl(x = all_data$class_ion, pattern = pattern_ACPIM)]),
             checkboxGroupInput(inputId = "select_PRL_class",
                                label = "Prenol lipids:",
                                choices = all_data$class_ion[grepl(x = all_data$class_ion, pattern = pattern_PRL)],
                                selected = all_data$class_ion[grepl(x = all_data$class_ion, pattern = pattern_PRL)])
      )
    )
  })

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

    # show histogram/violing plot
    show_rsd_lipidclass_violin(df = all_data$qc_results)
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

  # not used now
  # output$show_qc_table <- renderTable({
  #   req(all_data$qc_results,
  #       all_data$class_ion_selected)
  #
  #   all_data$qc_results %>%
  #     filter(.data$class_ion %in% all_data$class_ion_selected)
  # })

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
    input$select_PRL_class
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
                                     input$select_PRL_class,
                                     input$select_SA_class,
                                     input$select_STL_class
                                     )
    # how many lipid classes are selected
    all_data$num_lipid_classes <- length(unique(sapply(all_data$class_ion_selected, function(x) {
      unlist(strsplit(x = x,
                      split = " - "))[1]
    })))

    # filter the data, lipid classes are removed here
    # all_data$lipid_data_filter <- all_data$lipid_data_long %>%
    #   filter(.data$class_ion %in% all_data$class_ion_selected)

    # instead of removing them, tag them that I don't want them
    all_data$lipid_data_filter <- all_data$lipid_data_long %>%
      mutate(keep = if_else(.data$class_ion %in% all_data$class_ion_selected,
                            TRUE,
                            FALSE),
             comment = if_else(.data$class_ion %in% all_data$class_ion_selected,
                               NA_character_,
                               "remove_class"))

    all_data$clean_data <- all_data$clean_data %>%
      mutate(keep = if_else(.data$class_ion %in% all_data$class_ion_selected,
                            TRUE,
                            FALSE),
             comment = if_else(.data$class_ion %in% all_data$class_ion_selected,
                               NA_character_,
                               "remove_class"))
  },
  ignoreInit = TRUE)

  ### Fatty acids and conjugates
  filter_FA <- bubblePlotServer(id = "FA",
                                    data = reactive(all_data$lipid_data_filter),
                                    pattern = "^(Ox)?FA$",
                                    lipid_data = reactive(all_data$clean_data),
                                    title = input$navbar_selection)

  output$FA_UI <- renderUI({
    req(all_data$lipid_data_filter,
        all_data$clean_data)

    bubblePlotUI(id = "FA",
                 data = all_data$lipid_data_filter,
                 pattern = "^(Ox)?FA$")
  })

  observe({
    req(filter_FA)

    all_data$clean_data$keep[all_data$clean_data$my_id == filter_FA()$filter_data$my_id] <- filter_FA()$filter_data$keep
    all_data$clean_data$comment[all_data$clean_data$my_id == filter_FA()$filter_data$my_id] <- filter_FA()$filter_data$comment
  })
  ###

  ### Fatty amides
  filter_FAM <- bubblePlotServer(id = "FAM",
                   data = reactive(all_data$lipid_data_filter),
                   pattern = "^(NAGly|NAGlySer|NAOrn|NAE)",
                   lipid_data = reactive(all_data$clean_data),
                   title = input$navbar_selection)

  output$FAM_UI <- renderUI({
    req(all_data$lipid_data_filter,
        all_data$clean_data)

    bubblePlotUI(id = "FAM",
                 data = all_data$lipid_data_filter,
                 pattern = "^(NAGly|NAGlySer|NAOrn|NAE)")
  })

  observe({
    req(filter_FAM)

    all_data$clean_data$keep[all_data$clean_data$my_id == filter_FAM()$filter_data$my_id] <- filter_FAM()$filter_data$keep
    all_data$clean_data$comment[all_data$clean_data$my_id == filter_FAM()$filter_data$my_id] <- filter_FAM()$filter_data$comment
  })
  ###

  ### Fatty esters
  filter_FE <- bubblePlotServer(id = "FE",
                                data = reactive(all_data$lipid_data_filter),
                                pattern = "^(CAR|FAHFA)",
                                lipid_data = reactive(all_data$clean_data),
                                title = input$navbar_selection)

  output$FE_UI <- renderUI({
    req(all_data$lipid_data_filter,
        all_data$clean_data)

    bubblePlotUI(id = "FE",
                 data = all_data$lipid_data_filter,
                 pattern = "^(CAR|FAHFA)")
  })

  observe({
    req(filter_FE)

    all_data$clean_data$keep[all_data$clean_data$my_id == filter_FE()$filter_data$my_id] <- filter_FE()$filter_data$keep
    all_data$clean_data$comment[all_data$clean_data$my_id == filter_FE()$filter_data$my_id] <- filter_FE()$filter_data$comment
  })
  ###

  ### Ether glycerolipids
  filter_EGL <- bubblePlotServer(id = "EGL",
                                 data = reactive(all_data$lipid_data_filter),
                                 pattern = "^(Ether|Ox)[MDT]G$",
                                 lipid_data = reactive(all_data$clean_data),
                                 title = input$navbar_selection)

  output$EGL_UI <- renderUI({
    req(all_data$lipid_data_filter,
        all_data$clean_data)

      bubblePlotUI(id = "EGL",
                 data = all_data$lipid_data_filter,
                 pattern = "^(Ether|Ox)[MDT]G$")
  })

  observe({
    req(filter_EGL)

    all_data$clean_data$keep[all_data$clean_data$my_id == filter_EGL()$filter_data$my_id] <- filter_EGL()$filter_data$keep
    all_data$clean_data$comment[all_data$clean_data$my_id == filter_EGL()$filter_data$my_id] <- filter_EGL()$filter_data$comment
  })
  ###

  ### glycerolipids
  filter_GL <- bubblePlotServer(id = "GL",
                                data = reactive(all_data$lipid_data_filter),
                                pattern = "^[MDT]G$",
                                lipid_data = reactive(all_data$clean_data),
                                title = input$navbar_selection)

  output$GL_UI <- renderUI({
    req(all_data$lipid_data_filter,
        all_data$clean_data)

    bubblePlotUI(id = "GL",
                 data = all_data$lipid_data_filter,
                 pattern = "^[MDT]G$")
  })

  observe({
    req(filter_GL)

    all_data$clean_data$keep[all_data$clean_data$my_id == filter_GL()$filter_data$my_id] <- filter_GL()$filter_data$keep
    all_data$clean_data$comment[all_data$clean_data$my_id == filter_GL()$filter_data$my_id] <- filter_GL()$filter_data$comment
  })
  ###

  ### Glycosyldiradylglycerols
  filter_GLDG <- bubblePlotServer(id = "GLDG",
                                  data = reactive(all_data$lipid_data_filter),
                                  pattern = "^(Ether|EtherS)?[DMS][GQ]DG$",
                                  lipid_data = reactive(all_data$clean_data),
                                  title = input$navbar_selection)

  output$GLDG_UI <- renderUI({
    req(all_data$lipid_data_filter,
        all_data$clean_data)

    bubblePlotUI(id = "GLDG",
                 data = all_data$lipid_data_filter,
                 pattern = "^(Ether|EtherS)?[DMS][GQ]DG$")
  })

  observe({
    req(filter_GLDG)

    all_data$clean_data$keep[all_data$clean_data$my_id == filter_GLDG()$filter_data$my_id] <- filter_GLDG()$filter_data$keep
    all_data$clean_data$comment[all_data$clean_data$my_id == filter_GLDG()$filter_data$my_id] <- filter_GLDG()$filter_data$comment
  })
  ###

  ### Other glycerolipids
  filter_OGL <- bubblePlotServer(id = "OGL",
                                 data = reactive(all_data$lipid_data_filter),
                                 pattern = "^([AL]?DG(GA|CC|TS/A)|TG_EST)$",
                                 lipid_data = reactive(all_data$clean_data),
                                 title = input$navbar_selection)

  output$OGL_UI <- renderUI({
    req(all_data$lipid_data_filter,
        all_data$clean_data)

    bubblePlotUI(id = "OGL",
                 data = all_data$lipid_data_filter,
                 pattern = "^([AL]?DG(GA|CC|TS/A)|TG_EST)$")
  })

  observe({
    req(filter_OGL)

    all_data$clean_data$keep[all_data$clean_data$my_id == filter_OGL()$filter_data$my_id] <- filter_OGL()$filter_data$keep
    all_data$clean_data$comment[all_data$clean_data$my_id == filter_OGL()$filter_data$my_id] <- filter_OGL()$filter_data$comment
  })
  ###

  ### Glycerophosphates
  filter_PA <- bubblePlotServer(id = "PA",
                                data = reactive(all_data$lipid_data_filter),
                                pattern = "^L?PA$",
                                lipid_data = reactive(all_data$clean_data),
                                title = input$navbar_selection)

  output$PA_UI <- renderUI({
    req(all_data$lipid_data_filter,
        all_data$clean_data)

    bubblePlotUI(id = "PA",
                 data = all_data$lipid_data_filter,
                 pattern = "^L?PA$")
  })

  observe({
    req(filter_PA)

    all_data$clean_data$keep[all_data$clean_data$my_id == filter_PA()$filter_data$my_id] <- filter_PA()$filter_data$keep
    all_data$clean_data$comment[all_data$clean_data$my_id == filter_PA()$filter_data$my_id] <- filter_PA()$filter_data$comment
  })
  ###

  ### Glycerophosphocholines
  filter_PC <- bubblePlotServer(id = "PC",
                                data = reactive(all_data$lipid_data_filter),
                                pattern = "^(Ether)?L?PC$",
                                lipid_data = reactive(all_data$clean_data),
                                title = input$navbar_selection)

  output$PC_UI <- renderUI({
    req(all_data$lipid_data_filter,
        all_data$clean_data)

    bubblePlotUI(id = "PC",
                 data = all_data$lipid_data_filter,
                 pattern = "^(Ether)?L?PC$")
  })

  observe({
    req(filter_PC)

    all_data$clean_data$keep[all_data$clean_data$my_id == filter_PC()$filter_data$my_id] <- filter_PC()$filter_data$keep
    all_data$clean_data$comment[all_data$clean_data$my_id == filter_PC()$filter_data$my_id] <- filter_PC()$filter_data$comment
  })
  ###

  ### Glycerophosphocholines
  filter_PE <- bubblePlotServer(id = "PE",
                                data = reactive(all_data$lipid_data_filter),
                                pattern = "^(LNA)?(Ether)?L?PE(\\(P\\))?$",
                                lipid_data = reactive(all_data$clean_data),
                                title = input$navbar_selection)

  output$PE_UI <- renderUI({
    req(all_data$lipid_data_filter,
        all_data$clean_data)

    bubblePlotUI(id = "PE",
                 data = all_data$lipid_data_filter,
                 pattern = "^(LNA)?(Ether)?L?PE(\\(P\\))?$")
  })

  observe({
    req(filter_PE)

    all_data$clean_data$keep[all_data$clean_data$my_id == filter_PE()$filter_data$my_id] <- filter_PE()$filter_data$keep
    all_data$clean_data$comment[all_data$clean_data$my_id == filter_PE()$filter_data$my_id] <- filter_PE()$filter_data$comment
  })
  ###

  ### Glycerophosphoglycerols
  filter_PG <- bubblePlotServer(id = "PG",
                                data = reactive(all_data$lipid_data_filter),
                                pattern = "^(H?BMP|(Ether)?L?PG)$",
                                lipid_data = reactive(all_data$clean_data),
                                title = input$navbar_selection)

  output$PG_UI <- renderUI({
    req(all_data$lipid_data_filter,
        all_data$clean_data)

    bubblePlotUI(id = "PG",
                 data = all_data$lipid_data_filter,
                 pattern = "^(H?BMP|(Ether)?L?PG)$")
  })

  observe({
    req(filter_PG)

    all_data$clean_data$keep[all_data$clean_data$my_id == filter_PG()$filter_data$my_id] <- filter_PG()$filter_data$keep
    all_data$clean_data$comment[all_data$clean_data$my_id == filter_PG()$filter_data$my_id] <- filter_PG()$filter_data$comment
  })
  ###

  ### Glycerophosphoglycerophosphoglycerols (CL)
  filter_CL <- bubblePlotServer(id = "CL",
                                data = reactive(all_data$lipid_data_filter),
                                pattern = "^([DM]L)?CL$",
                                lipid_data = reactive(all_data$clean_data),
                                title = input$navbar_selection)

  output$CL_UI <- renderUI({
    req(all_data$lipid_data_filter,
        all_data$clean_data)

    bubblePlotUI(id = "CL",
                 data = all_data$lipid_data_filter,
                 pattern = "^([DM]L)?CL$")
  })

  observe({
    req(filter_CL)

    all_data$clean_data$keep[all_data$clean_data$my_id == filter_CL()$filter_data$my_id] <- filter_CL()$filter_data$keep
    all_data$clean_data$comment[all_data$clean_data$my_id == filter_CL()$filter_data$my_id] <- filter_CL()$filter_data$comment
  })
  ###

  ### Glycerophosphoinositolglycans
  filter_AcPIM <- bubblePlotServer(id = "AcPIM",
                                   data = reactive(all_data$lipid_data_filter),
                                   pattern = "^Ac[2-4]PIM[12]$",
                                   lipid_data = reactive(all_data$clean_data),
                                   title = input$navbar_selection)

  output$AcPIM_UI <- renderUI({
    req(all_data$lipid_data_filter,
        all_data$clean_data)

    bubblePlotUI(id = "AcPIM",
                 data = all_data$lipid_data_filter,
                 pattern = "^Ac[2-4]PIM[12]$")
  })

  observe({
    req(filter_AcPIM)

    all_data$clean_data$keep[all_data$clean_data$my_id == filter_AcPIM()$filter_data$my_id] <- filter_AcPIM()$filter_data$keep
    all_data$clean_data$comment[all_data$clean_data$my_id == filter_AcPIM()$filter_data$my_id] <- filter_AcPIM()$filter_data$comment
  })
  ###

  ### Glycerophosphoglycerols
  filter_PI <- bubblePlotServer(id = "PI",
                                data = reactive(all_data$lipid_data_filter),
                                pattern = "^(Ether)?L?PI$",
                                lipid_data = reactive(all_data$clean_data),
                                title = input$navbar_selection)

  output$PI_UI <- renderUI({
    req(all_data$lipid_data_filter,
        all_data$clean_data)

    bubblePlotUI(id = "PI",
                 data = all_data$lipid_data_filter,
                 pattern = "^(Ether)?L?PI$")
  })

  observe({
    req(filter_PI)

    all_data$clean_data$keep[all_data$clean_data$my_id == filter_PI()$filter_data$my_id] <- filter_PI()$filter_data$keep
    all_data$clean_data$comment[all_data$clean_data$my_id == filter_PI()$filter_data$my_id] <- filter_PI()$filter_data$comment
  })
  ###

  ### Glycerophosphoserines
  filter_PS <- bubblePlotServer(id = "PS",
                                data = reactive(all_data$lipid_data_filter),
                                pattern = "^(LNA)?(Ether)?L?PS$",
                                lipid_data = reactive(all_data$clean_data),
                                title = input$navbar_selection)

  output$PS_UI <- renderUI({
    req(all_data$lipid_data_filter,
        all_data$clean_data)

    bubblePlotUI(id = "PS",
                 data = all_data$lipid_data_filter,
                 pattern = "^(LNA)?(Ether)?L?PS$")
  })

  observe({
    req(filter_PS)

    all_data$clean_data$keep[all_data$clean_data$my_id == filter_PS()$filter_data$my_id] <- filter_PS()$filter_data$keep
    all_data$clean_data$comment[all_data$clean_data$my_id == filter_PS()$filter_data$my_id] <- filter_PS()$filter_data$comment
  })
  ###

  ### oxidized glycerophospholipids
  filter_OPL <- bubblePlotServer(id = "OPL",
                                 data = reactive(all_data$lipid_data_filter),
                                 pattern = "^OxP[ACEGIS]$",
                                 lipid_data = reactive(all_data$clean_data),
                                 title = input$navbar_selection)

  output$OPL_UI <- renderUI({
    req(all_data$lipid_data_filter,
        all_data$clean_data)

    bubblePlotUI(id = "OPL",
                 data = all_data$lipid_data_filter,
                 pattern = "^OxP[ACEGIS]$")
  })

  observe({
    req(filter_OPL)

    all_data$clean_data$keep[all_data$clean_data$my_id == filter_OPL()$filter_data$my_id] <- filter_OPL()$filter_data$keep
    all_data$clean_data$comment[all_data$clean_data$my_id == filter_OPL()$filter_data$my_id] <- filter_OPL()$filter_data$comment
  })
  ###

  ### Other Glycerophospholipids
  filter_OGPL <- bubblePlotServer(id = "OGPL",
                                  data = reactive(all_data$lipid_data_filter),
                                  pattern = "^P(Et|Me)OH$",
                                  lipid_data = reactive(all_data$clean_data),
                                  title = input$navbar_selection)

  output$OGPL_UI <- renderUI({
    req(all_data$lipid_data_filter,
        all_data$clean_data)

    bubblePlotUI(id = "OGPL",
                 data = all_data$lipid_data_filter,
                 pattern = "^P(Et|Me)OH$")
  })

  observe({
    req(filter_OGPL)

    all_data$clean_data$keep[all_data$clean_data$my_id == filter_OGPL()$filter_data$my_id] <- filter_OGPL()$filter_data$keep
    all_data$clean_data$comment[all_data$clean_data$my_id == filter_OGPL()$filter_data$my_id] <- filter_OGPL()$filter_data$comment
  })
  ###

  ### Prenol lipids
  filter_PRL <- bubblePlotServer(id = "PRL",
                                 data = reactive(all_data$lipid_data_filter),
                                 pattern = "^(VAE|CoQ|VitaminE)$",
                                 lipid_data = reactive(all_data$clean_data),
                                 title = input$navbar_selection)

  output$PRL_UI <- renderUI({
    req(all_data$lipid_data_filter,
        all_data$clean_data)

    bubblePlotUI(id = "PRL",
                 data = all_data$lipid_data_filter,
                 pattern = "^(VAE|CoQ|VitaminE)$")
  })

  observe({
    req(filter_PRL)

    all_data$clean_data$keep[all_data$clean_data$my_id == filter_PRL()$filter_data$my_id] <- filter_PRL()$filter_data$keep
    all_data$clean_data$comment[all_data$clean_data$my_id == filter_PRL()$filter_data$my_id] <- filter_PRL()$filter_data$comment
  })
  ###

  ### Acidic glycosphingolipids
  filter_AcGL <- bubblePlotServer(id = "AcGL",
                                  data = reactive(all_data$lipid_data_filter),
                                  pattern = "^(GM3|SHexCer(\\+O)?)$",
                                  lipid_data = reactive(all_data$clean_data),
                                  title = input$navbar_selection)

  output$AcGL_UI <- renderUI({
    req(all_data$lipid_data_filter,
        all_data$clean_data)

    bubblePlotUI(id = "AcGL",
                 data = all_data$lipid_data_filter,
                 pattern = "^(GM3|SHexCer(\\+O)?)$")
  })

  observe({
    req(filter_AcGL)

    all_data$clean_data$keep[all_data$clean_data$my_id == filter_AcGL()$filter_data$my_id] <- filter_AcGL()$filter_data$keep
    all_data$clean_data$comment[all_data$clean_data$my_id == filter_AcGL()$filter_data$my_id] <- filter_AcGL()$filter_data$comment
  })
  ###

  ### Ceramides
  filter_Cer <- bubblePlotServer(id = "Cer",
                                 data = reactive(all_data$lipid_data_filter),
                                 pattern = "^Cer[P_]",
                                 lipid_data = reactive(all_data$clean_data),
                                 title = input$navbar_selection)

  output$Cer_UI <- renderUI({
    req(all_data$lipid_data_filter,
        all_data$clean_data)

    bubblePlotUI(id = "Cer",
                 data = all_data$lipid_data_filter,
                 pattern = "^Cer[P_]")
  })

  observe({
    req(filter_Cer)

    all_data$clean_data$keep[all_data$clean_data$my_id == filter_Cer()$filter_data$my_id] <- filter_Cer()$filter_data$keep
    all_data$clean_data$comment[all_data$clean_data$my_id == filter_Cer()$filter_data$my_id] <- filter_Cer()$filter_data$comment
  })
  ###

  ### phosphosphingolipids
  filter_PSL <- bubblePlotServer(id = "PSL",
                                 data = reactive(all_data$lipid_data_filter),
                                 pattern = "^(ASM|PE_Cer(\\+O)?|PI_Cer(\\+O)?|SM|SM\\+O)",
                                 lipid_data = reactive(all_data$clean_data),
                                 title = input$navbar_selection)

  output$PSL_UI <- renderUI({
    req(all_data$lipid_data_filter,
        all_data$clean_data)

    bubblePlotUI(id = "PSL",
                 data = all_data$lipid_data_filter,
                 pattern = "^(ASM|PE_Cer(\\+O)?|PI_Cer(\\+O)?|SM|SM\\+O)")
  })

  observe({
    req(filter_PSL)

    all_data$clean_data$keep[all_data$clean_data$my_id == filter_PSL()$filter_data$my_id] <- filter_PSL()$filter_data$keep
    all_data$clean_data$comment[all_data$clean_data$my_id == filter_PSL()$filter_data$my_id] <- filter_PSL()$filter_data$comment
  })
  ###

  ### Neutral glycosphingolipids
  filter_NPSL <- bubblePlotServer(id = "NPSL",
                                  data = reactive(all_data$lipid_data_filter),
                                  pattern = "^A?Hex[23]?Cer",
                                  lipid_data = reactive(all_data$clean_data),
                                  title = input$navbar_selection)

  output$NPSL_UI <- renderUI({
    req(all_data$lipid_data_filter,
        all_data$clean_data)

    bubblePlotUI(id = "NPSL",
                 data = all_data$lipid_data_filter,
                 pattern = "^A?Hex[23]?Cer")
  })

  observe({
    req(filter_NPSL)

    all_data$clean_data$keep[all_data$clean_data$my_id == filter_NPSL()$filter_data$my_id] <- filter_NPSL()$filter_data$keep
    all_data$clean_data$comment[all_data$clean_data$my_id == filter_NPSL()$filter_data$my_id] <- filter_NPSL()$filter_data$comment
  })
  ###

  ### Sphingoid bases
  filter_SB <- bubblePlotServer(id = "SB",
                                data = reactive(all_data$lipid_data_filter),
                                pattern = "^((Phyto|DH)?Sph|SL(\\+O)?)$",
                                lipid_data = reactive(all_data$clean_data),
                                title = input$navbar_selection)

  output$SB_UI <- renderUI({
    req(all_data$lipid_data_filter,
        all_data$clean_data)

    bubblePlotUI(id = "SB",
                 data = all_data$lipid_data_filter,
                 pattern = "^((Phyto|DH)?Sph|SL(\\+O)?)$")
  })

  observe({
    req(filter_SB)

    all_data$clean_data$keep[all_data$clean_data$my_id == filter_SB()$filter_data$my_id] <- filter_SB()$filter_data$keep
    all_data$clean_data$comment[all_data$clean_data$my_id == filter_SB()$filter_data$my_id] <- filter_SB()$filter_data$comment
  })
  ###

  ### Bile acids and conjugates
  filter_BA <- bubblePlotServer(id = "BA",
                                data = reactive(all_data$lipid_data_filter),
                                pattern = "^(BASulfate|BileAcid|DCAE)$",
                                lipid_data = reactive(all_data$clean_data),
                                title = input$navbar_selection)

  output$BA_UI <- renderUI({
    req(all_data$lipid_data_filter,
        all_data$clean_data)

    bubblePlotUI(id = "BA",
                 data = all_data$lipid_data_filter,
                 pattern = "^(BASulfate|BileAcid|DCAE)$")
  })

  observe({
    req(filter_BA)

    all_data$clean_data$keep[all_data$clean_data$my_id == filter_BA()$filter_data$my_id] <- filter_BA()$filter_data$keep
    all_data$clean_data$comment[all_data$clean_data$my_id == filter_BA()$filter_data$my_id] <- filter_BA()$filter_data$comment
  })
  ###

  ### Secosteroids
  filter_SC <- bubblePlotServer(id = "SC",
                                data = reactive(all_data$lipid_data_filter),
                                pattern = "^VitaminD$",
                                lipid_data = reactive(all_data$clean_data),
                                title = input$navbar_selection)

  output$SC_UI <- renderUI({
    req(all_data$lipid_data_filter,
        all_data$clean_data)

    bubblePlotUI(id = "SC",
                 data = all_data$lipid_data_filter,
                 pattern = "^VitaminD$")
  })

  observe({
    req(filter_SC)

    all_data$clean_data$keep[all_data$clean_data$my_id == filter_SC()$filter_data$my_id] <- filter_SC()$filter_data$keep
    all_data$clean_data$comment[all_data$clean_data$my_id == filter_SC()$filter_data$my_id] <- filter_SC()$filter_data$comment
  })
  ###

  ### Steroid conjugates
  filter_STC <- bubblePlotServer(id = "STC",
                                 data = reactive(all_data$lipid_data_filter),
                                 pattern = "^SSulfate$",
                                 lipid_data = reactive(all_data$clean_data),
                                 title = input$navbar_selection)

  output$STC_UI <- renderUI({
    req(all_data$lipid_data_filter,
        all_data$clean_data)

    bubblePlotUI(id = "STC",
                 data = all_data$lipid_data_filter,
                 pattern = "^SSulfate$")
  })

  observe({
    req(filter_STC)

    all_data$clean_data$keep[all_data$clean_data$my_id == filter_STC()$filter_data$my_id] <- filter_STC()$filter_data$keep
    all_data$clean_data$comment[all_data$clean_data$my_id == filter_STC()$filter_data$my_id] <- filter_STC()$filter_data$comment
  })
  ###

  ### Sterols
  filter_ST <- bubblePlotServer(id = "ST",
                                data = reactive(all_data$lipid_data_filter),
                                pattern = "^((BR|CA|SI|ST)?[CS]E|Cholesterol|SHex|ST)$",
                                lipid_data = reactive(all_data$clean_data),
                                title = input$navbar_selection)

  output$ST_UI <- renderUI({
    req(all_data$lipid_data_filter,
        all_data$clean_data)

    bubblePlotUI(id = "ST",
                 data = all_data$lipid_data_filter,
                 pattern = "^((BR|CA|SI|ST)?[CS]E|Cholesterol|SHex)$")
  })

  observe({
    req(filter_ST)

    all_data$clean_data$keep[all_data$clean_data$my_id == filter_ST()$filter_data$my_id] <- filter_ST()$filter_data$keep
    all_data$clean_data$comment[all_data$clean_data$my_id == filter_ST()$filter_data$my_id] <- filter_ST()$filter_data$comment
  })
  ###

  ### Other sterol lipids
  filter_OST <- bubblePlotServer(id = "OST",
                                 data = reactive(all_data$lipid_data_filter),
                                 pattern = "^AHex(CAS|CS|SIS|BRS|STS)$",
                                 lipid_data = reactive(all_data$clean_data),
                                 title = input$navbar_selection)

  output$OST_UI <- renderUI({
    req(all_data$lipid_data_filter,
        all_data$clean_data)

    bubblePlotUI(id = "OST",
                 data = all_data$lipid_data_filter,
                 pattern = "^AHex(CAS|CS|SIS|BRS|STS)$")
  })

  observe({
    req(filter_OST)

    all_data$clean_data$keep[all_data$clean_data$my_id == filter_OST()$filter_data$my_id] <- filter_OST()$filter_data$keep
    all_data$clean_data$comment[all_data$clean_data$my_id == filter_OST()$filter_data$my_id] <- filter_OST()$filter_data$comment
  })
  ###

  ### Show the issues
  output$tbl_issues <- renderTable({
    req(all_data$clean_data)

    all_data$clean_data %>%
      filter(.data$keep == FALSE) %>%
      select(.data$my_id:.data$polarity, -.data$scale_DotProduct, -.data$scale_RevDotProduct, .data$keep, .data$comment)
  })

  #### About / Help  section ####
  output$about_session <- renderPrint({
    session_info()
  })

  # for debugging: check which lipid classes / ions are selected
  # output$lipid_classes <- renderText({
  #   req(all_data$class_ion_selected)
  #
  #   all_data$class_ion_selected
  # })

  # output$debug <- renderTable({
  #   req(filter_result)
  #   filter_result()$filter_data
  # })
}
