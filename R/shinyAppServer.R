#' @title Shiny app server function
#'
#' @description This is the server function to run the shiny app.
#'
#' @param input provided by shiny
#' @param output provided by shiny
#' @param session provided by shiny
#'
#' @import shiny
#' @importFrom shinyjs toggle
#' @importFrom sessioninfo session_info
#' @importFrom tibble tibble
#' @importFrom dplyr filter mutate select pull distinct
#' @importFrom rlang .data
#' @importFrom purrr map
#' @importFrom magrittr %>%
#' @importFrom tidyr unnest
#' @importFrom utils head
#' @importFrom tools file_ext
#' @importFrom readxl read_xlsx
#' @importFrom DT renderDT
#' @importFrom plotly renderPlotly plotlyOutput
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
                             analysis_data = NULL,
                             meta_data = NULL,
                             merged_data = NULL,
                             qc_results = NULL,
                             class_ion = NULL,
                             class_ion_selected = NULL,
                             num_lipid_classes = NULL,
                             samples_selected = NULL)

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

    # cleanup some column names
    results <- clean_up(lipid_data = results)

    # keep only the identified lipids and sort by lipid class, lipid
    all_data$lipid_data <- select_identified(lipid_data = results)

    # make a copy of the original data and work with this
    all_data$clean_data <- all_data$lipid_data

    # make the data long
    all_data$lipid_data_long <- tidy_lipids(lipid_data = all_data$lipid_data)

    # initialize a data frame for filtering
    all_data$lipid_data_filter <- all_data$lipid_data_long

    # get all lipid classes with their respective ion
    all_data$class_ion <- all_data$lipid_data_long %>%
      pull(.data$class_ion) %>%
      unique()

    all_data$class_ion_selected <- all_data$class_ion

    # calculate the RSD values
    all_data$qc_results <- calc_rsd(lipid_data = all_data$lipid_data_long)
  })

  # show the raw data
  output$lipid_data_table <- renderDT({
    req(all_data$lipid_data)

    all_data$lipid_data %>%
      # remove a few columns
      select(-.data$MSMSspectrum, -.data$scale_DotProduct, -.data$scale_RevDotProduct, -.data$keep, -.data$comment)
  },
  options = list(pageLength = 10,
                 lengthChange = FALSE,
                 dom = "pt",
                 ordering = TRUE,
                 autoWidth = TRUE),
  selection = "none",
  filter = "top",
  rownames = FALSE)

  #### info msdial files
  # show/hide info about msdial files
  observeEvent(input$btn_info_msdial, {
    toggle(id = "info_msdial")
  })

  # the text to show
  output$info_msdial <- renderUI({
    tagList(
      column(width = 3,
             p("You can load the exported result files from MS-DIAL here (>= v4.60). There
               should be a separate file for positive and negative mode.")
      )
    )
  })

  # show/hide info about meta data files
  observeEvent(input$btn_info_meta, {
    toggle(id = "info_meta")
  })

  # the text to show
  output$info_meta <- renderUI({
    tagList(
      column(width = 3,
             p("The meta data file should be an Excel (.xlsx) file. The file must contain
               a column with the filenames used. Please don't use any spaces in the column
               names!!")
      )
    )
  })
  ####

  #### select samples
  # show the checkboxes for (de-)selecting samples
  output$samples_list <- renderUI({
    req(all_data$lipid_data_long)

    # get all the sample/qcpool names
    all_sample_names <- all_data$lipid_data_long %>%
      pull(.data$sample_name) %>%
      unique() %>%
      sort()

    tagList(
      checkboxGroupInput(inputId = "select_samples",
                         label = "(De-)select samples:",
                         choices = all_sample_names,
                         selected = all_sample_names)
    )
  })

  # do the actual "removing" of the samples
  observeEvent(input$select_samples, {
    req(all_data$lipid_data_long)

    # get everything which is selected
    all_data$samples_selected <- input$select_samples
  })
  ####

  #### Select lipid classes ####
  # get the lipid classes
  output$select_lipid_classes <- renderUI({
    req(all_data$lipid_data_long)

    # regular expression patterns
    pattern_PL <- "^((Ether)?(Ox)?(L)?(LNA)?(MM)?P[ACEGISM]|HBMP|BMP)"
    pattern_GL <- "^(Ox|Ether|SQ|EtherS|L|A)?[DMT]G"
    pattern_Cer <- "^Cer[P_]"
    pattern_HexCer <- "^A?Hex[23]?Cer"
    pattern_FA <- "^((Ox)?FA|FAHFA|NAGly|NAGlySer|NAOrn|NAE|CAR)"
    pattern_PSL <- "^(ASM|PE_Cer(\\+O)?|PI_Cer(\\+O)?|SM|SM\\+O)"
    pattern_SB <- "^(PhytoSph|SL|SL\\+O|DHSph|Sph)"
    pattern_SA <- "^(GM3|SHexCer|SHexCer\\+O)"
    pattern_CL <- "^([DM]L)?CL"
    pattern_ACPIM <- "^Ac[2-4]PIM[12]"
    pattern_STL <- "^((BA|S)Sulfate|BileAcid|AHex[BCS][AIRTS][S]?|(BRS|CAS|C|SIS|STS|DCA|TDCA)E|SHex|Cholesterol|VitaminD|ST) "
    pattern_PRL <- "^(VAE|CoQ|VitaminE)"

    my_col_width <- 3

    tagList(
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
    show_rsd_histogram(qc_data = all_data$qc_results)
  })

  # create histogram of all lipids per lipid class
  output$rsd_lipid_classes <- renderPlot({
    req(all_data$qc_results)

    # show histogram/violing plot
    show_rsd_lipidclass_violin(qc_data = all_data$qc_results)
  })

  # create the output UI
  output$rsd_lipidclass_ui <- renderUI({
    req(all_data$qc_results)

    # if the number of lipid classes is not calculated yet, do it here
    if(is.null(all_data$num_lipid_classes)) {
      all_data$num_lipid_classes <- length(unique(sapply(all_data$class_ion_selected, function(x) {
        unlist(strsplit(x = x,
                        split = " - "))[1]
      })))
    }

    # calculate the new height for the violin plot
    new_height <- ceiling(all_data$num_lipid_classes * 25)

    tagList(
      plotOutput(outputId = "rsd_lipid_classes",
                 width = "50%",
                 height = paste0(new_height, "px"))
    )
  })

  output$create_corplot <- renderPlotly({
    req(all_data$lipid_data)

    cor_heatmap2(lipid_data = all_data$lipid_data)
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
      plotlyOutput(outputId = "create_corplot",
                   width = "50%",
                   height = paste0(new_height, "px"))
    )
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

    # instead of removing them, tag them that I don't want them
    all_data$lipid_data_filter <- all_data$lipid_data_long %>%
      mutate(keep = if_else(.data$class_ion %in% all_data$class_ion_selected,
                            TRUE,
                            FALSE),
             comment = if_else(.data$class_ion %in% all_data$class_ion_selected,
                               "",
                               "remove_class"))

    # Do this here as well
    all_data$clean_data <- all_data$clean_data %>%
      mutate(keep = if_else(.data$class_ion %in% all_data$class_ion_selected,
                            TRUE,
                            FALSE),
             comment = if_else(.data$class_ion %in% all_data$class_ion_selected,
                               "",
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
    req(filter_FA,
        all_data$clean_data)

    if(nrow(filter_FA()$filter_data) > 0) {
      all_data$clean_data$keep[all_data$clean_data$my_id == filter_FA()$filter_data$my_id] <- filter_FA()$filter_data$keep
      all_data$clean_data$comment[all_data$clean_data$my_id == filter_FA()$filter_data$my_id] <- filter_FA()$filter_data$comment
      all_data$clean_data$ShortLipidName[all_data$clean_data$my_id == filter_FA()$filter_data$my_id] <- rename_lipid(lipid_data = all_data$clean_data,
                                                                                                                     rename_info = filter_FA()$filter_data)
    }
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
    req(filter_FAM,
        all_data$clean_data)

    if(nrow(filter_FAM()$filter_data) > 0) {
      all_data$clean_data$keep[all_data$clean_data$my_id == filter_FAM()$filter_data$my_id] <- filter_FAM()$filter_data$keep
      all_data$clean_data$comment[all_data$clean_data$my_id == filter_FAM()$filter_data$my_id] <- filter_FAM()$filter_data$comment
      all_data$clean_data$ShortLipidName[all_data$clean_data$my_id == filter_FAM()$filter_data$my_id] <- rename_lipid(lipid_data = all_data$clean_data,
                                                                                                                      rename_info = filter_FAM()$filter_data)
    }
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
    req(filter_FE,
        all_data$clean_data)

    if(nrow(filter_FE()$filter_data) > 0) {
      all_data$clean_data$keep[all_data$clean_data$my_id == filter_FE()$filter_data$my_id] <- filter_FE()$filter_data$keep
      all_data$clean_data$comment[all_data$clean_data$my_id == filter_FE()$filter_data$my_id] <- filter_FE()$filter_data$comment
      all_data$clean_data$ShortLipidName[all_data$clean_data$my_id == filter_FE()$filter_data$my_id] <- rename_lipid(lipid_data = all_data$clean_data,
                                                                                                                     rename_info = filter_FE()$filter_data)
    }
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
    req(filter_EGL,
        all_data$clean_data)

    if(nrow(filter_EGL()$filter_data) > 0) {
      all_data$clean_data$keep[all_data$clean_data$my_id == filter_EGL()$filter_data$my_id] <- filter_EGL()$filter_data$keep
      all_data$clean_data$comment[all_data$clean_data$my_id == filter_EGL()$filter_data$my_id] <- filter_EGL()$filter_data$comment
      all_data$clean_data$ShortLipidName[all_data$clean_data$my_id == filter_EGL()$filter_data$my_id] <- rename_lipid(lipid_data = all_data$clean_data,
                                                                                                                      rename_info = filter_EGL()$filter_data)
    }
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
    req(filter_GL,
        all_data$clean_data)

    if(nrow(filter_GL()$filter_data) > 0) {
      all_data$clean_data$keep[all_data$clean_data$my_id == filter_GL()$filter_data$my_id] <- filter_GL()$filter_data$keep
      all_data$clean_data$comment[all_data$clean_data$my_id == filter_GL()$filter_data$my_id] <- filter_GL()$filter_data$comment
      all_data$clean_data$ShortLipidName[all_data$clean_data$my_id == filter_GL()$filter_data$my_id] <- rename_lipid(lipid_data = all_data$clean_data,
                                                                                                                     rename_info = filter_GL()$filter_data)
    }
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
    req(filter_GLDG,
        all_data$clean_data)

    if(nrow(filter_GLDG()$filter_data) > 0) {
      all_data$clean_data$keep[all_data$clean_data$my_id == filter_GLDG()$filter_data$my_id] <- filter_GLDG()$filter_data$keep
      all_data$clean_data$comment[all_data$clean_data$my_id == filter_GLDG()$filter_data$my_id] <- filter_GLDG()$filter_data$comment
      all_data$clean_data$ShortLipidName[all_data$clean_data$my_id == filter_GLDG()$filter_data$my_id] <- rename_lipid(lipid_data = all_data$clean_data,
                                                                                                                       rename_info = filter_GLDG()$filter_data)
    }
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
    req(filter_OGL,
        all_data$clean_data)

    if(nrow(filter_OGL()$filter_data) > 0) {
      all_data$clean_data$keep[all_data$clean_data$my_id == filter_OGL()$filter_data$my_id] <- filter_OGL()$filter_data$keep
      all_data$clean_data$comment[all_data$clean_data$my_id == filter_OGL()$filter_data$my_id] <- filter_OGL()$filter_data$comment
      all_data$clean_data$ShortLipidName[all_data$clean_data$my_id == filter_OGL()$filter_data$my_id] <- rename_lipid(lipid_data = all_data$clean_data,
                                                                                                                      rename_info = filter_OGL()$filter_data)
    }
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
    req(filter_PA,
        all_data$clean_data)

    if(nrow(filter_PA()$filter_data) > 0) {
      all_data$clean_data$keep[all_data$clean_data$my_id == filter_PA()$filter_data$my_id] <- filter_PA()$filter_data$keep
      all_data$clean_data$comment[all_data$clean_data$my_id == filter_PA()$filter_data$my_id] <- filter_PA()$filter_data$comment
      all_data$clean_data$ShortLipidName[all_data$clean_data$my_id == filter_PA()$filter_data$my_id] <- rename_lipid(lipid_data = all_data$clean_data,
                                                                                                                     rename_info = filter_PA()$filter_data)
    }
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
    req(filter_PC,
        all_data$clean_data)

    if(nrow(filter_PC()$filter_data) > 0) {
      all_data$clean_data$keep[all_data$clean_data$my_id == filter_PC()$filter_data$my_id] <- filter_PC()$filter_data$keep
      all_data$clean_data$comment[all_data$clean_data$my_id == filter_PC()$filter_data$my_id] <- filter_PC()$filter_data$comment
      all_data$clean_data$ShortLipidName[all_data$clean_data$my_id == filter_PC()$filter_data$my_id] <- rename_lipid(lipid_data = all_data$clean_data,
                                                                                                                     rename_info = filter_PC()$filter_data)
    }
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
    req(filter_PE,
        all_data$clean_data)

    if(nrow(filter_PE()$filter_data) > 0) {
      all_data$clean_data$keep[all_data$clean_data$my_id == filter_PE()$filter_data$my_id] <- filter_PE()$filter_data$keep
      all_data$clean_data$comment[all_data$clean_data$my_id == filter_PE()$filter_data$my_id] <- filter_PE()$filter_data$comment
      all_data$clean_data$ShortLipidName[all_data$clean_data$my_id == filter_PE()$filter_data$my_id] <- rename_lipid(lipid_data = all_data$clean_data,
                                                                                                                     rename_info = filter_PE()$filter_data)
    }
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
    req(filter_PG,
        all_data$clean_data)

    if(nrow(filter_PE()$filter_data) > 0) {
      all_data$clean_data$keep[all_data$clean_data$my_id == filter_PG()$filter_data$my_id] <- filter_PG()$filter_data$keep
      all_data$clean_data$comment[all_data$clean_data$my_id == filter_PG()$filter_data$my_id] <- filter_PG()$filter_data$comment
      all_data$clean_data$ShortLipidName[all_data$clean_data$my_id == filter_PG()$filter_data$my_id] <- rename_lipid(lipid_data = all_data$clean_data,
                                                                                                                     rename_info = filter_PG()$filter_data)
    }
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
    req(filter_CL,
        all_data$clean_data)

    if(nrow(filter_CL()$filter_data) > 0) {
      all_data$clean_data$keep[all_data$clean_data$my_id == filter_CL()$filter_data$my_id] <- filter_CL()$filter_data$keep
      all_data$clean_data$comment[all_data$clean_data$my_id == filter_CL()$filter_data$my_id] <- filter_CL()$filter_data$comment
      all_data$clean_data$ShortLipidName[all_data$clean_data$my_id == filter_CL()$filter_data$my_id] <- rename_lipid(lipid_data = all_data$clean_data,
                                                                                                                     rename_info = filter_CL()$filter_data)
    }
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
    req(filter_AcPIM,
        all_data$clean_data)

    if(nrow(filter_AcPIM()$filter_data) > 0) {
      all_data$clean_data$keep[all_data$clean_data$my_id == filter_AcPIM()$filter_data$my_id] <- filter_AcPIM()$filter_data$keep
      all_data$clean_data$comment[all_data$clean_data$my_id == filter_AcPIM()$filter_data$my_id] <- filter_AcPIM()$filter_data$comment
      all_data$clean_data$ShortLipidName[all_data$clean_data$my_id == filter_AcPIM()$filter_data$my_id] <- rename_lipid(lipid_data = all_data$clean_data,
                                                                                                                        rename_info = filter_AcPIM()$filter_data)
    }
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
    req(filter_PI,
        all_data$clean_data)

    if(nrow(filter_PI()$filter_data) > 0) {
      all_data$clean_data$keep[all_data$clean_data$my_id == filter_PI()$filter_data$my_id] <- filter_PI()$filter_data$keep
      all_data$clean_data$comment[all_data$clean_data$my_id == filter_PI()$filter_data$my_id] <- filter_PI()$filter_data$comment
      all_data$clean_data$ShortLipidName[all_data$clean_data$my_id == filter_PI()$filter_data$my_id] <- rename_lipid(lipid_data = all_data$clean_data,
                                                                                                                     rename_info = filter_PI()$filter_data)
    }
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
    req(filter_PS,
        all_data$clean_data)

    if(nrow(filter_PS()$filter_data) > 0) {
      all_data$clean_data$keep[all_data$clean_data$my_id == filter_PS()$filter_data$my_id] <- filter_PS()$filter_data$keep
      all_data$clean_data$comment[all_data$clean_data$my_id == filter_PS()$filter_data$my_id] <- filter_PS()$filter_data$comment
      all_data$clean_data$ShortLipidName[all_data$clean_data$my_id == filter_PS()$filter_data$my_id] <- rename_lipid(lipid_data = all_data$clean_data,
                                                                                                                     rename_info = filter_PS()$filter_data)
    }
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
    req(filter_OPL,
        all_data$clean_data)

    if(nrow(filter_OPL()$filter_data) > 0) {
      all_data$clean_data$keep[all_data$clean_data$my_id == filter_OPL()$filter_data$my_id] <- filter_OPL()$filter_data$keep
      all_data$clean_data$comment[all_data$clean_data$my_id == filter_OPL()$filter_data$my_id] <- filter_OPL()$filter_data$comment
      all_data$clean_data$ShortLipidName[all_data$clean_data$my_id == filter_OPL()$filter_data$my_id] <- rename_lipid(lipid_data = all_data$clean_data,
                                                                                                                      rename_info = filter_OPL()$filter_data)
    }
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
    req(filter_OGPL,
        all_data$clean_data)

    if(nrow(filter_OGPL()$filter_data) > 0) {
      all_data$clean_data$keep[all_data$clean_data$my_id == filter_OGPL()$filter_data$my_id] <- filter_OGPL()$filter_data$keep
      all_data$clean_data$comment[all_data$clean_data$my_id == filter_OGPL()$filter_data$my_id] <- filter_OGPL()$filter_data$comment
      all_data$clean_data$ShortLipidName[all_data$clean_data$my_id == filter_OGPL()$filter_data$my_id] <- rename_lipid(lipid_data = all_data$clean_data,
                                                                                                                       rename_info = filter_OGPL()$filter_data)
    }
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
    req(filter_PRL,
        all_data$clean_data)

    if(nrow(filter_PRL()$filter_data) > 0) {
      all_data$clean_data$keep[all_data$clean_data$my_id == filter_PRL()$filter_data$my_id] <- filter_PRL()$filter_data$keep
      all_data$clean_data$comment[all_data$clean_data$my_id == filter_PRL()$filter_data$my_id] <- filter_PRL()$filter_data$comment
      all_data$clean_data$ShortLipidName[all_data$clean_data$my_id == filter_PRL()$filter_data$my_id] <- rename_lipid(lipid_data = all_data$clean_data,
                                                                                                                      rename_info = filter_PRL()$filter_data)
    }
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
    req(filter_AcGL,
        all_data$clean_data)

    if(nrow(filter_AcGL()$filter_data) > 0) {
      all_data$clean_data$keep[all_data$clean_data$my_id == filter_AcGL()$filter_data$my_id] <- filter_AcGL()$filter_data$keep
      all_data$clean_data$comment[all_data$clean_data$my_id == filter_AcGL()$filter_data$my_id] <- filter_AcGL()$filter_data$comment
      all_data$clean_data$ShortLipidName[all_data$clean_data$my_id == filter_AcGL()$filter_data$my_id] <- rename_lipid(lipid_data = all_data$clean_data,
                                                                                                                       rename_info = filter_AcGL()$filter_data)
    }
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
    req(filter_Cer,
        all_data$clean_data)

    if(nrow(filter_Cer()$filter_data) > 0) {
      all_data$clean_data$keep[all_data$clean_data$my_id == filter_Cer()$filter_data$my_id] <- filter_Cer()$filter_data$keep
      all_data$clean_data$comment[all_data$clean_data$my_id == filter_Cer()$filter_data$my_id] <- filter_Cer()$filter_data$comment
      all_data$clean_data$ShortLipidName[all_data$clean_data$my_id == filter_Cer()$filter_data$my_id] <- rename_lipid(lipid_data = all_data$clean_data,
                                                                                                                      rename_info = filter_Cer()$filter_data)
    }
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
    req(filter_PSL,
        all_data$clean_data)

    if(nrow(filter_PSL()$filter_data) > 0) {
      all_data$clean_data$keep[all_data$clean_data$my_id == filter_PSL()$filter_data$my_id] <- filter_PSL()$filter_data$keep
      all_data$clean_data$comment[all_data$clean_data$my_id == filter_PSL()$filter_data$my_id] <- filter_PSL()$filter_data$comment
      all_data$clean_data$ShortLipidName[all_data$clean_data$my_id == filter_PSL()$filter_data$my_id] <- rename_lipid(lipid_data = all_data$clean_data,
                                                                                                                      rename_info = filter_PSL()$filter_data)
    }
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
    req(filter_NPSL,
        all_data$clean_data)

    if(nrow(filter_NPSL()$filter_data) > 0) {
      all_data$clean_data$keep[all_data$clean_data$my_id == filter_NPSL()$filter_data$my_id] <- filter_NPSL()$filter_data$keep
      all_data$clean_data$comment[all_data$clean_data$my_id == filter_NPSL()$filter_data$my_id] <- filter_NPSL()$filter_data$comment
      all_data$clean_data$ShortLipidName[all_data$clean_data$my_id == filter_NPSL()$filter_data$my_id] <- rename_lipid(lipid_data = all_data$clean_data,
                                                                                                                       rename_info = filter_NPSL()$filter_data)
    }
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
    req(filter_SB,
        all_data$clean_data)

    if(nrow(filter_SB()$filter_data) > 0) {
      all_data$clean_data$keep[all_data$clean_data$my_id == filter_SB()$filter_data$my_id] <- filter_SB()$filter_data$keep
      all_data$clean_data$comment[all_data$clean_data$my_id == filter_SB()$filter_data$my_id] <- filter_SB()$filter_data$comment
      all_data$clean_data$ShortLipidName[all_data$clean_data$my_id == filter_SB()$filter_data$my_id] <- rename_lipid(lipid_data = all_data$clean_data,
                                                                                                                     rename_info = filter_SB()$filter_data)
    }
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
    req(filter_BA,
        all_data$clean_data)

    if(nrow(filter_BA()$filter_data) > 0) {
      all_data$clean_data$keep[all_data$clean_data$my_id == filter_BA()$filter_data$my_id] <- filter_BA()$filter_data$keep
      all_data$clean_data$comment[all_data$clean_data$my_id == filter_BA()$filter_data$my_id] <- filter_BA()$filter_data$comment
      all_data$clean_data$ShortLipidName[all_data$clean_data$my_id == filter_BA()$filter_data$my_id] <- rename_lipid(lipid_data = all_data$clean_data,
                                                                                                                     rename_info = filter_BA()$filter_data)
    }
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
    req(filter_SC,
        all_data$clean_data)

    if(nrow(filter_SC()$filter_data) > 0) {
      all_data$clean_data$keep[all_data$clean_data$my_id == filter_SC()$filter_data$my_id] <- filter_SC()$filter_data$keep
      all_data$clean_data$comment[all_data$clean_data$my_id == filter_SC()$filter_data$my_id] <- filter_SC()$filter_data$comment
      all_data$clean_data$ShortLipidName[all_data$clean_data$my_id == filter_SC()$filter_data$my_id] <- rename_lipid(lipid_data = all_data$clean_data,
                                                                                                                     rename_info = filter_SC()$filter_data)
    }
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
    req(filter_STC,
        all_data$clean_data)

    if(nrow(filter_STC()$filter_data) > 0) {
      all_data$clean_data$keep[all_data$clean_data$my_id == filter_STC()$filter_data$my_id] <- filter_STC()$filter_data$keep
      all_data$clean_data$comment[all_data$clean_data$my_id == filter_STC()$filter_data$my_id] <- filter_STC()$filter_data$comment
      all_data$clean_data$ShortLipidName[all_data$clean_data$my_id == filter_STC()$filter_data$my_id] <- rename_lipid(lipid_data = all_data$clean_data,
                                                                                                                      rename_info = filter_STC()$filter_data)
    }
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
    req(filter_ST,
        all_data$clean_data)

    if(nrow(filter_ST()$filter_data) > 0) {
      all_data$clean_data$keep[all_data$clean_data$my_id == filter_ST()$filter_data$my_id] <- filter_ST()$filter_data$keep
      all_data$clean_data$comment[all_data$clean_data$my_id == filter_ST()$filter_data$my_id] <- filter_ST()$filter_data$comment
      all_data$clean_data$ShortLipidName[all_data$clean_data$my_id == filter_ST()$filter_data$my_id] <- rename_lipid(lipid_data = all_data$clean_data,
                                                                                                                     rename_info = filter_ST()$filter_data)
    }
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
    req(filter_OST,
        all_data$clean_data)

    if(nrow(filter_OST()$filter_data) > 0) {
      all_data$clean_data$keep[all_data$clean_data$my_id == filter_OST()$filter_data$my_id] <- filter_OST()$filter_data$keep
      all_data$clean_data$comment[all_data$clean_data$my_id == filter_OST()$filter_data$my_id] <- filter_OST()$filter_data$comment
      all_data$clean_data$ShortLipidName[all_data$clean_data$my_id == filter_OST()$filter_data$my_id] <- rename_lipid(lipid_data = all_data$clean_data,
                                                                                                                      rename_info = filter_OST()$filter_data)
    }
  })
  ###
  ####

  ### Show the issues
  output$tbl_issues <- renderTable({
    req(all_data$clean_data)

    all_data$clean_data %>%
      filter(.data$keep == FALSE |
               (.data$keep == TRUE & .data$comment == "rename"),
             .data$comment != "remove_class") %>%
      select(.data$my_id:.data$polarity, -.data$scale_DotProduct, -.data$scale_RevDotProduct, .data$keep, .data$comment)
  })

  output$tbl_issues_class <- renderTable({
    req(all_data$clean_data)

    all_data$clean_data %>%
      filter(.data$keep == FALSE,
             .data$comment == "remove_class") %>%
      select(.data$LipidClass, .data$class_ion, .data$keep, .data$comment) %>%
      distinct(.data$class_ion,
               .keep_all = TRUE)
  })
  #### eind issues part

  #### meta data part
  # get the file
  xlsx_file <- reactive({
    req(input$meta_data_file)
    # If no file is selected, don't do anything
    # validate(need(input$meta_data_file, message = FALSE))

    # check if it is an xlsx file
    my_file <- input$meta_data_file
    ext <- file_ext(my_file$datapath)
    validate(need(ext == "xlsx", "Please upload a .xlsx file!"))

    return(input$meta_data_file)
  })

  # read the meta data file
  all_data$meta_data <- reactive({
    read_xlsx(path = xlsx_file()$datapath)
  })

  # show the content of the meta data file
  output$show_meta_data <- renderDT({
    req(all_data$meta_data)

    all_data$meta_data()
  },
  options = list(pageLength = 10,
                 lengthChange = FALSE,
                 dom = "pt",
                 ordering = TRUE),
  selection = "none")

  # generate the UI part for column selecting and merging
  output$merge_ui <- renderUI({
    req(all_data$meta_data)

    # get the column names of the meta data
    merge_colnames <- colnames(all_data$meta_data())

    tagList(
      # create pull down list for column selection
      selectInput(inputId = "select_meta_column",
                  label = "Select column for merging:",
                  choices = c("none", merge_colnames),
                  selected = "none"),
      # show status on which column the merge was done
      htmlOutput(outputId = "status_merge"),
      # the merge button
      actionButton(inputId = "btn_merge_meta",
                   label = "Merge")
    )
  })

  # show status on which column the merge was done
  output$status_merge <- renderUI({
    req(input$btn_merge_meta)

    # make sure this doesn't change if only a new column is selected.
    selected_column <- isolate(input$select_meta_column)
    # nothing should happen if the selected column is 'none'
    if(selected_column != "none") {
      p("Data merged on column ", HTML("<b>"), selected_column, HTML("</b>"), ".")
    }
  })

  observeEvent(input$btn_merge_meta, {
    req(all_data$clean_data,
        all_data$meta_data)

    # make sure this doesn't change if only a new column is selected.
    selected_column <- isolate(input$select_meta_column)
    # nothing should happen if the selected column is 'none'
    if(selected_column != "none") {
      all_data$merged_data <- merge_data(lipid_data = all_data$clean_data,
                                         meta_data = all_data$meta_data(),
                                         by = selected_column)
    } else {
      all_data$merged_data <- NULL
    }
  })

  # show the merged data
  output$show_merged_data <- renderDT({
    req(all_data$merged_data)

    all_data$merged_data
  },
  options = list(pageLength = 10,
                 lengthChange = FALSE,
                 dom = "pt"),
  selection = "none")
  #### end meta merge part

  # keep on eye on if the data gets merged
  observe({
    req(all_data$clean_data)

    if(!is.null(all_data$merged_data)) {
      # data is merged
      all_data$analysis_data <- all_data$merged_data
    } else {
      # data is not merged
      all_data$analysis_data <- merge_data(lipid_data = all_data$clean_data)
    }

    # isolate the analysis data, because it needs to overwritten
    tmp_data <- isolate(all_data$analysis_data)
    # remove the unwanted samples
    if(!is.null(all_data$samples_selected)) {
      # several samples removed
      all_data$analysis_data <- tmp_data %>%
        # make sure not to overwrite lipids which are already on FALSE
        mutate(keep = if_else(.data$sample_name %in% all_data$samples_selected &
                                .data$keep == TRUE,
                              TRUE,
                              FALSE),
               comment = if_else(.data$sample_name %in% all_data$samples_selected &
                                   .data$keep == TRUE,
                                 "",
                                 "remove_sample"))
    } else {
      # no samples removed
      all_data$analyis_data <- tmp_data
    }
  })

  #### Analysis part
  output$compare_samples <- renderPlotly({
    req(all_data$analysis_data)

    compare_samples_heatmap(lipid_data = all_data$analysis_data)
  })

  #### end analysis part

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
