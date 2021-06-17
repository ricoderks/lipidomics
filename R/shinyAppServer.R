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
#' @importFrom dplyr filter mutate select pull distinct case_when
#' @importFrom rlang .data
#' @importFrom purrr map
#' @importFrom magrittr %>%
#' @importFrom tidyr unnest pivot_wider
#' @importFrom utils head
#' @importFrom tools file_ext
#' @importFrom readxl read_xlsx
#' @importFrom DT renderDT
#' @importFrom plotly renderPlotly plotlyOutput
#'
#' @author Rico Derks

# Define server logic
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
                             merged_data = FALSE,
                             qc_results = NULL,
                             class_ion = NULL,
                             class_ion_selected = NULL,
                             num_lipid_classes = NULL,
                             all_samples = NULL,
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

    # make the data long
    all_data$lipid_data_long <- tidy_lipids(lipid_data = all_data$lipid_data)

    ### can't this be simpler??

    # get all lipid classes with their respective ion
    all_data$class_ion <- all_data$lipid_data_long %>%
      distinct(.data$class_ion) %>%
      pull(.data$class_ion)

    # store them
    all_data$class_ion_selected <- all_data$class_ion

    # get all sample name
    all_data$all_samples <- all_data$lipid_data_long %>%
      distinct(.data$sample_name) %>%
      pull(.data$sample_name)

    all_data$samples_selected <- all_data$all_samples

    # calculate the RSD values
    all_data$qc_results <- calc_rsd(lipid_data = all_data$lipid_data_long)

    # tag lipids which have a too high RSD value
    # find the lipids to keep
    keep_lipids_rsd <- all_data$qc_results %>%
      filter(.data$rsd_area <= 0.3) %>%
      distinct(.data$my_id) %>%
      pull(.data$my_id)

    keep_lipids_msms <- all_data$lipid_data %>%
      filter(!(.data$DotProduct <= 50 &
                 .data$RevDotProduct <= 50 &
                 .data$keep == TRUE)) %>%
      distinct(.data$my_id) %>%
      pull(.data$my_id)

    all_data$lipid_data_filter <- all_data$lipid_data_long %>%
      mutate(
        keep = case_when(
          !(.data$my_id %in% keep_lipids_rsd) ~ FALSE,
          !(.data$my_id %in% keep_lipids_msms) ~ FALSE,
          TRUE ~ TRUE),
        comment = case_when(
          !(.data$my_id %in% keep_lipids_rsd) ~ "large_rsd",
          !(.data$my_id %in% keep_lipids_msms) ~ "no_match",
          TRUE ~ "")
      )
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

  # show/hide info about QC filter
  observeEvent(input$btn_info_qcfilter, {
    toggle(id = "info_qcfilter")
  })

  # the text to show
  output$info_qcfilter <- renderUI({
    tagList(
      column(width = 3,
             p("Lipids are immediately tagged with `large_rsd` and will not show up in
               the bubble plots (identification tab) or in the analysis part.")
      )
    )
  })

  # show/hide info about dot product filter
  observeEvent(input$btn_info_prodfilter, {
    toggle(id = "info_prodfilter")
  })

  # the text to show
  output$info_prodfilter <- renderUI({
    tagList(
      column(width = 3,
             p("Lipids are immediately tagged with `no_match` and will not show up in
               the bubble plots (identification tab) or in the analysis part. Individual
               lipids can be added back via the bubble plots (identification part). Keep
               in mind that when the value of this filter is changed they might be removed again!!")
      )
    )
  })
  ####

  #### select samples
  # show the checkboxes for (de-)selecting samples
  output$samples_list <- renderUI({
    req(all_data$lipid_data_long)

    tagList(
      checkboxGroupInput(inputId = "select_samples",
                         label = "(De-)select samples:",
                         choices = all_data$all_samples,
                         selected = all_data$all_samples)
    )
  })

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
    req(all_data$lipid_data_long,
        input$rsd_cutoff)

    # show histogram
    show_rsd_histogram(qc_data = all_data$qc_results,
                       rsd = input$rsd_cutoff)
  })

  # create histogram/violin plot of all lipids per lipid class
  output$rsd_lipid_classes <- renderPlot({
    req(all_data$qc_results,
        input$rsd_cutoff)

    # show histogram/violing plot
    show_rsd_lipidclass_violin(qc_data = all_data$qc_results,
                               rsd = input$rsd_cutoff)
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
      distinct(.data$sample_name) %>%
      pull(.data$sample_name) %>%
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
  observeEvent(input$select_samples, {
    req(input$select_samples)

    # store which samples are selected
    all_data$samples_selected <- input$select_samples
  })

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
    input$rsd_cutoff
    input$dotprod_cutoff
    input$revdotprod_cutoff
  }, {
    req(input$rsd_cutoff,
        input$dotprod_cutoff,
        input$revdotprod_cutoff,
        all_data$qc_results)
    # get all the selected classes
    class_ion_selected <- c(input$select_PL_class,
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
                            input$select_STL_class)

    # if they where not visible yet, keep what was already there
    if(!any(is.null(class_ion_selected))) {
      all_data$class_ion_selected <- class_ion_selected
    }

    # how many lipid classes are selected
    all_data$num_lipid_classes <- length(unique(sapply(all_data$class_ion_selected, function(x) {
      unlist(strsplit(x = x,
                      split = " - "))[1]
    })))

    tmp_filter <- isolate(all_data$lipid_data_filter)

    # which lipids have a low RSD
    keep_lipids_rsd <- all_data$qc_results %>%
      filter(.data$rsd_area <= input$rsd_cutoff) %>%
      distinct(.data$my_id) %>%
      pull(.data$my_id)

    # which lipids have a high dotproduct and revdotproduct
    keep_lipids_msms <- tmp_filter %>%
      filter(!(.data$DotProduct <= input$dotprod_cutoff &
                 .data$RevDotProduct <= input$revdotprod_cutoff &
                 .data$comment != "large_rsd")) %>%
      distinct(.data$my_id) %>%
      pull(.data$my_id)

    # get the id's to keep lipids which lipid class is selected
    keep_lipids_class <- tmp_filter %>%
      filter(.data$class_ion %in% all_data$class_ion_selected) %>%
      distinct(.data$my_id) %>%
      pull(.data$my_id)

    tmp_filter <- tmp_filter %>%
      mutate(
        keep = case_when(
          !(.data$my_id %in% keep_lipids_rsd) ~ FALSE,
          !(.data$my_id %in% keep_lipids_msms) ~ FALSE,
          # !(.data$class_ion %in% all_data$class_ion_selected) ~ FALSE,
          # !(.data$my_id %in% keep_lipids_class) ~ FALSE,
          TRUE ~ TRUE),
        comment = case_when(
          !(.data$my_id %in% keep_lipids_rsd) ~ "large_rsd",
          !(.data$my_id %in% keep_lipids_msms) ~ "no_match",
          # !(.data$class_ion %in% all_data$class_ion_selected) ~ "remove_class",
          # !(.data$my_id %in% keep_lipids_class) ~ "remove_class",
          TRUE ~ "")
      )

    # this is needed to remove a class completely
    all_data$lipid_data_filter <- tmp_filter %>%
      mutate(keep = if_else(!(.data$my_id %in% keep_lipids_class),
                            FALSE,
                            .data$keep),
             comment = if_else(!(.data$my_id %in% keep_lipids_class),
                               "remove_class",
                               .data$comment))
  },
  ignoreInit = TRUE)

  ### Fatty acids and conjugates
  filter_FA <- bubblePlotServer(id = "FA",
                                lipid_data = reactive(all_data$lipid_data_filter),
                                pattern = "^(Ox)?FA$",
                                title = input$navbar_selection)

  output$FA_UI <- renderUI({
    req(all_data$lipid_data_filter)

    bubblePlotUI(id = "FA",
                 data = all_data$lipid_data_filter,
                 pattern = "^(Ox)?FA$")
  })

  observe({
    req(filter_FA,
        all_data$lipid_data_filter)

    if(nrow(filter_FA()$filter_data) > 0) {
      all_data$lipid_data_filter <- set_issue_info(lipid_data = isolate(all_data$lipid_data_filter),
                                                   info = isolate(filter_FA()))
    }
  })
  ###

  ### Fatty amides
  filter_FAM <- bubblePlotServer(id = "FAM",
                                 lipid_data = reactive(all_data$lipid_data_filter),
                                 pattern = "^(NAGly|NAGlySer|NAOrn|NAE)",
                                 title = input$navbar_selection)

  output$FAM_UI <- renderUI({
    req(all_data$lipid_data_filter)

    bubblePlotUI(id = "FAM",
                 data = all_data$lipid_data_filter,
                 pattern = "^(NAGly|NAGlySer|NAOrn|NAE)")
  })

  observe({
    req(filter_FAM)

    if(nrow(filter_FAM()$filter_data) > 0) {
      all_data$lipid_data_filter <- set_issue_info(lipid_data = isolate(all_data$lipid_data_filter),
                                                   info = isolate(filter_FAM()))
    }
  })
  ###

  ### Fatty esters
  filter_FE <- bubblePlotServer(id = "FE",
                                lipid_data = reactive(all_data$lipid_data_filter),
                                pattern = "^(CAR|FAHFA)",
                                title = input$navbar_selection)

  output$FE_UI <- renderUI({
    req(all_data$lipid_data_filter)

    bubblePlotUI(id = "FE",
                 data = all_data$lipid_data_filter,
                 pattern = "^(CAR|FAHFA)")
  })

  observe({
    req(filter_FE)

    if(nrow(filter_FE()$filter_data) > 0) {
      all_data$lipid_data_filter <- set_issue_info(lipid_data = isolate(all_data$lipid_data_filter),
                                                   info = isolate(filter_FE()))
    }
  })
  ###

  ### Ether glycerolipids
  filter_EGL <- bubblePlotServer(id = "EGL",
                                 lipid_data = reactive(all_data$lipid_data_filter),
                                 pattern = "^(Ether|Ox)[MDT]G$",
                                 title = input$navbar_selection)

  output$EGL_UI <- renderUI({
    req(all_data$lipid_data_filter)

    bubblePlotUI(id = "EGL",
                 data = all_data$lipid_data_filter,
                 pattern = "^(Ether|Ox)[MDT]G$")
  })

  observe({
    req(filter_EGL)

    if(nrow(filter_EGL()$filter_data) > 0) {
      all_data$lipid_data_filter <- set_issue_info(lipid_data = isolate(all_data$lipid_data_filter),
                                                   info = isolate(filter_EGL()))
    }
  })
  ###

  ### glycerolipids
  filter_GL <- bubblePlotServer(id = "GL",
                                lipid_data = reactive(all_data$lipid_data_filter),
                                pattern = "^[MDT]G$",
                                title = input$navbar_selection)

  output$GL_UI <- renderUI({
    req(all_data$lipid_data_filter)

    bubblePlotUI(id = "GL",
                 data = all_data$lipid_data_filter,
                 pattern = "^[MDT]G$")
  })

  observe({
    req(filter_GL)

    if(nrow(filter_GL()$filter_data) > 0) {
      all_data$lipid_data_filter <- set_issue_info(lipid_data = isolate(all_data$lipid_data_filter),
                                                   info = isolate(filter_GL()))
    }
  })
  ###

  ### Glycosyldiradylglycerols
  filter_GLDG <- bubblePlotServer(id = "GLDG",
                                  lipid_data = reactive(all_data$lipid_data_filter),
                                  pattern = "^(Ether|EtherS)?[DMS][GQ]DG$",
                                  title = input$navbar_selection)

  output$GLDG_UI <- renderUI({
    req(all_data$lipid_data_filter)

    bubblePlotUI(id = "GLDG",
                 data = all_data$lipid_data_filter,
                 pattern = "^(Ether|EtherS)?[DMS][GQ]DG$")
  })

  observe({
    req(filter_GLDG)

    if(nrow(filter_GLDG()$filter_data) > 0) {
      all_data$lipid_data_filter <- set_issue_info(lipid_data = isolate(all_data$lipid_data_filter),
                                                   info = isolate(filter_GLDG()))
    }
  })
  ###

  ### Other glycerolipids
  filter_OGL <- bubblePlotServer(id = "OGL",
                                 lipid_data = reactive(all_data$lipid_data_filter),
                                 pattern = "^([AL]?DG(GA|CC|TS/A)|TG_EST)$",
                                 title = input$navbar_selection)

  output$OGL_UI <- renderUI({
    req(all_data$lipid_data_filter)

    bubblePlotUI(id = "OGL",
                 data = all_data$lipid_data_filter,
                 pattern = "^([AL]?DG(GA|CC|TS/A)|TG_EST)$")
  })

  observe({
    req(filter_OGL)

    if(nrow(filter_OGL()$filter_data) > 0) {
      all_data$lipid_data_filter <- set_issue_info(lipid_data = isolate(all_data$lipid_data_filter),
                                                   info = isolate(filter_OGL()))
    }
  })
  ###

  ### Glycerophosphates
  filter_PA <- bubblePlotServer(id = "PA",
                                lipid_data = reactive(all_data$lipid_data_filter),
                                pattern = "^L?PA$",
                                title = input$navbar_selection)

  output$PA_UI <- renderUI({
    req(all_data$lipid_data_filter)

    bubblePlotUI(id = "PA",
                 data = all_data$lipid_data_filter,
                 pattern = "^L?PA$")
  })

  observe({
    req(filter_PA)

    if(nrow(filter_PA()$filter_data) > 0) {
      all_data$lipid_data_filter <- set_issue_info(lipid_data = isolate(all_data$lipid_data_filter),
                                                   info = isolate(filter_PA()))
    }
  })
  ###

  ### Glycerophosphocholines
  filter_PC <- bubblePlotServer(id = "PC",
                                lipid_data = reactive(all_data$lipid_data_filter),
                                pattern = "^(Ether)?L?PC$",
                                title = input$navbar_selection)

  output$PC_UI <- renderUI({
    req(all_data$lipid_data_filter)

    bubblePlotUI(id = "PC",
                 data = all_data$lipid_data_filter,
                 pattern = "^(Ether)?L?PC$")
  })

  observe({
    req(filter_PC)

    if(nrow(filter_PC()$filter_data) > 0) {
      all_data$lipid_data_filter <- set_issue_info(lipid_data = isolate(all_data$lipid_data_filter),
                                                   info = isolate(filter_PC()))
    }
  })
  ###

  ### Glycerophosphocholines
  filter_PE <- bubblePlotServer(id = "PE",
                                lipid_data = reactive(all_data$lipid_data_filter),
                                pattern = "^(LNA)?(Ether)?L?PE(\\(P\\))?$",
                                title = input$navbar_selection)

  output$PE_UI <- renderUI({
    req(all_data$lipid_data_filtera)

    bubblePlotUI(id = "PE",
                 data = all_data$lipid_data_filter,
                 pattern = "^(LNA)?(Ether)?L?PE(\\(P\\))?$")
  })

  observe({
    req(filter_PE)

    if(nrow(filter_PE()$filter_data) > 0) {
      all_data$lipid_data_filter <- set_issue_info(lipid_data = isolate(all_data$lipid_data_filter),
                                                   info = isolate(filter_PE()))
    }
  })
  ###

  ### Glycerophosphoglycerols
  filter_PG <- bubblePlotServer(id = "PG",
                                lipid_data = reactive(all_data$lipid_data_filter),
                                pattern = "^(H?BMP|(Ether)?L?PG)$",
                                title = input$navbar_selection)

  output$PG_UI <- renderUI({
    req(all_data$lipid_data_filter)

    bubblePlotUI(id = "PG",
                 data = all_data$lipid_data_filter,
                 pattern = "^(H?BMP|(Ether)?L?PG)$")
  })

  observe({
    req(filter_PG)

    if(nrow(filter_PE()$filter_data) > 0) {
      all_data$lipid_data_filter <- set_issue_info(lipid_data = isolate(all_data$lipid_data_filter),
                                                   info = isolate(filter_PG()))
    }
  })
  ###

  ### Glycerophosphoglycerophosphoglycerols (CL)
  filter_CL <- bubblePlotServer(id = "CL",
                                lipid_data = reactive(all_data$lipid_data_filter),
                                pattern = "^([DM]L)?CL$",
                                title = input$navbar_selection)

  output$CL_UI <- renderUI({
    req(all_data$lipid_data_filter)

    bubblePlotUI(id = "CL",
                 data = all_data$lipid_data_filter,
                 pattern = "^([DM]L)?CL$")
  })

  observe({
    req(filter_CL)

    if(nrow(filter_CL()$filter_data) > 0) {
      all_data$lipid_data_filter <- set_issue_info(lipid_data = isolate(all_data$lipid_data_filter),
                                                   info = isolate(filter_CL()))
    }
  })
  ###

  ### Glycerophosphoinositolglycans
  filter_AcPIM <- bubblePlotServer(id = "AcPIM",
                                   lipid_data = reactive(all_data$lipid_data_filter),
                                   pattern = "^Ac[2-4]PIM[12]$",
                                   title = input$navbar_selection)

  output$AcPIM_UI <- renderUI({
    req(all_data$lipid_data_filtera)

    bubblePlotUI(id = "AcPIM",
                 data = all_data$lipid_data_filter,
                 pattern = "^Ac[2-4]PIM[12]$")
  })

  observe({
    req(filter_AcPIM)

    if(nrow(filter_AcPIM()$filter_data) > 0) {
      all_data$lipid_data_filter <- set_issue_info(lipid_data = isolate(all_data$lipid_data_filter),
                                                   info = isolate(filter_AcPIM()))
    }
  })
  ###

  ### Glycerophosphoglycerols
  filter_PI <- bubblePlotServer(id = "PI",
                                lipid_data = reactive(all_data$lipid_data_filter),
                                pattern = "^(Ether)?L?PI$",
                                title = input$navbar_selection)

  output$PI_UI <- renderUI({
    req(all_data$lipid_data_filter)

    bubblePlotUI(id = "PI",
                 data = all_data$lipid_data_filter,
                 pattern = "^(Ether)?L?PI$")
  })

  observe({
    req(filter_PI)

    if(nrow(filter_PI()$filter_data) > 0) {
      all_data$lipid_data_filter <- set_issue_info(lipid_data = isolate(all_data$lipid_data_filter),
                                                   info = isolate(filter_PI()))
    }
  })
  ###

  ### Glycerophosphoserines
  filter_PS <- bubblePlotServer(id = "PS",
                                lipid_data = reactive(all_data$lipid_data_filter),
                                pattern = "^(LNA)?(Ether)?L?PS$",
                                title = input$navbar_selection)

  output$PS_UI <- renderUI({
    req(all_data$lipid_data_filter)

    bubblePlotUI(id = "PS",
                 data = all_data$lipid_data_filter,
                 pattern = "^(LNA)?(Ether)?L?PS$")
  })

  observe({
    req(filter_PS)

    if(nrow(filter_PS()$filter_data) > 0) {
      all_data$lipid_data_filter <- set_issue_info(lipid_data = isolate(all_data$lipid_data_filter),
                                                   info = isolate(filter_PS()))
    }
  })
  ###

  ### oxidized glycerophospholipids
  filter_OPL <- bubblePlotServer(id = "OPL",
                                 lipid_data = reactive(all_data$lipid_data_filter),
                                 pattern = "^OxP[ACEGIS]$",
                                 title = input$navbar_selection)

  output$OPL_UI <- renderUI({
    req(all_data$lipid_data_filter)

    bubblePlotUI(id = "OPL",
                 data = all_data$lipid_data_filter,
                 pattern = "^OxP[ACEGIS]$")
  })

  observe({
    req(filter_OPL)

    if(nrow(filter_OPL()$filter_data) > 0) {
      all_data$lipid_data_filter <- set_issue_info(lipid_data = isolate(all_data$lipid_data_filter),
                                                   info = isolate(filter_OPL()))
    }
  })
  ###

  ### Other Glycerophospholipids
  filter_OGPL <- bubblePlotServer(id = "OGPL",
                                  lipid_data = reactive(all_data$lipid_data_filter),
                                  pattern = "^P(Et|Me)OH$",
                                  title = input$navbar_selection)

  output$OGPL_UI <- renderUI({
    req(all_data$lipid_data_filter)

    bubblePlotUI(id = "OGPL",
                 data = all_data$lipid_data_filter,
                 pattern = "^P(Et|Me)OH$")
  })

  observe({
    req(filter_OGPL)

    if(nrow(filter_OGPL()$filter_data) > 0) {
      all_data$lipid_data_filter <- set_issue_info(lipid_data = isolate(all_data$lipid_data_filter),
                                                   info = isolate(filter_OGPL()))
    }
  })
  ###

  ### Prenol lipids
  filter_PRL <- bubblePlotServer(id = "PRL",
                                 lipid_data = reactive(all_data$lipid_data_filter),
                                 pattern = "^(VAE|CoQ|VitaminE)$",
                                 title = input$navbar_selection)

  output$PRL_UI <- renderUI({
    req(all_data$lipid_data_filter)

    bubblePlotUI(id = "PRL",
                 data = all_data$lipid_data_filter,
                 pattern = "^(VAE|CoQ|VitaminE)$")
  })

  observe({
    req(filter_PRL)

    if(nrow(filter_PRL()$filter_data) > 0) {
      all_data$lipid_data_filter <- set_issue_info(lipid_data = isolate(all_data$lipid_data_filter),
                                                   info = isolate(filter_PRL()))
    }
  })
  ###

  ### Acidic glycosphingolipids
  filter_AcGL <- bubblePlotServer(id = "AcGL",
                                  lipid_data = reactive(all_data$lipid_data_filter),
                                  pattern = "^(GM3|SHexCer(\\+O)?)$",
                                  title = input$navbar_selection)

  output$AcGL_UI <- renderUI({
    req(all_data$lipid_data_filter)

    bubblePlotUI(id = "AcGL",
                 data = all_data$lipid_data_filter,
                 pattern = "^(GM3|SHexCer(\\+O)?)$")
  })

  observe({
    req(filter_AcGL)

    if(nrow(filter_AcGL()$filter_data) > 0) {
      all_data$lipid_data_filter <- set_issue_info(lipid_data = isolate(all_data$lipid_data_filter),
                                                   info = isolate(filter_AcGL()))
    }
  })
  ###

  ### Ceramides
  filter_Cer <- bubblePlotServer(id = "Cer",
                                 lipid_data = reactive(all_data$lipid_data_filter),
                                 pattern = "^Cer[P_]",
                                 title = input$navbar_selection)

  output$Cer_UI <- renderUI({
    req(all_data$lipid_data_filter)

    bubblePlotUI(id = "Cer",
                 data = all_data$lipid_data_filter,
                 pattern = "^Cer[P_]")
  })

  observe({
    req(filter_Cer)

    if(nrow(filter_Cer()$filter_data) > 0) {
      all_data$lipid_data_filter <- set_issue_info(lipid_data = isolate(all_data$lipid_data_filter),
                                                   info = isolate(filter_Cer()))
    }
  })
  ###

  ### phosphosphingolipids
  filter_PSL <- bubblePlotServer(id = "PSL",
                                 lipid_data = reactive(all_data$lipid_data_filter),
                                 pattern = "^(ASM|PE_Cer(\\+O)?|PI_Cer(\\+O)?|SM|SM\\+O)",
                                 title = input$navbar_selection)

  output$PSL_UI <- renderUI({
    req(all_data$lipid_data_filter)

    bubblePlotUI(id = "PSL",
                 data = all_data$lipid_data_filter,
                 pattern = "^(ASM|PE_Cer(\\+O)?|PI_Cer(\\+O)?|SM|SM\\+O)")
  })

  observe({
    req(filter_PSL)

    if(nrow(filter_PSL()$filter_data) > 0) {
      all_data$lipid_data_filter <- set_issue_info(lipid_data = isolate(all_data$lipid_data_filter),
                                                   info = isolate(filter_PSL()))
    }
  })
  ###

  ### Neutral glycosphingolipids
  filter_NPSL <- bubblePlotServer(id = "NPSL",
                                  lipid_data = reactive(all_data$lipid_data_filter),
                                  pattern = "^A?Hex[23]?Cer",
                                  title = input$navbar_selection)

  output$NPSL_UI <- renderUI({
    req(all_data$lipid_data_filter)

    bubblePlotUI(id = "NPSL",
                 data = all_data$lipid_data_filter,
                 pattern = "^A?Hex[23]?Cer")
  })

  observe({
    req(filter_NPSL)

    if(nrow(filter_NPSL()$filter_data) > 0) {
      all_data$lipid_data_filter <- set_issue_info(lipid_data = isolate(all_data$lipid_data_filter),
                                                   info = isolate(filter_NPSL()))
    }
  })
  ###

  ### Sphingoid bases
  filter_SB <- bubblePlotServer(id = "SB",
                                lipid_data = reactive(all_data$lipid_data_filter),
                                pattern = "^((Phyto|DH)?Sph|SL(\\+O)?)$",
                                title = input$navbar_selection)

  output$SB_UI <- renderUI({
    req(all_data$lipid_data_filter)

    bubblePlotUI(id = "SB",
                 data = all_data$lipid_data_filter,
                 pattern = "^((Phyto|DH)?Sph|SL(\\+O)?)$")
  })

  observe({
    req(filter_SB)

    if(nrow(filter_SB()$filter_data) > 0) {
      all_data$lipid_data_filter <- set_issue_info(lipid_data = isolate(all_data$lipid_data_filter),
                                                   info = isolate(filter_SB()))
    }
  })
  ###

  ### Bile acids and conjugates
  filter_BA <- bubblePlotServer(id = "BA",
                                lipid_data = reactive(all_data$lipid_data_filter),
                                pattern = "^(BASulfate|BileAcid|DCAE)$",
                                title = input$navbar_selection)

  output$BA_UI <- renderUI({
    req(all_data$lipid_data_filter)

    bubblePlotUI(id = "BA",
                 data = all_data$lipid_data_filter,
                 pattern = "^(BASulfate|BileAcid|DCAE)$")
  })

  observe({
    req(filter_BA)

    if(nrow(filter_BA()$filter_data) > 0) {
      all_data$lipid_data_filter <- set_issue_info(lipid_data = isolate(all_data$lipid_data_filter),
                                                   info = isolate(filter_BA()))
    }
  })
  ###

  ### Secosteroids
  filter_SC <- bubblePlotServer(id = "SC",
                                lipid_data = reactive(all_data$lipid_data_filter),
                                pattern = "^VitaminD$",
                                title = input$navbar_selection)

  output$SC_UI <- renderUI({
    req(all_data$lipid_data_filter)

    bubblePlotUI(id = "SC",
                 data = all_data$lipid_data_filter,
                 pattern = "^VitaminD$")
  })

  observe({
    req(filter_SC)

    if(nrow(filter_SC()$filter_data) > 0) {
      all_data$lipid_data_filter <- set_issue_info(lipid_data = isolate(all_data$lipid_data_filter),
                                                   info = isolate(filter_SC()))
    }
  })
  ###

  ### Steroid conjugates
  filter_STC <- bubblePlotServer(id = "STC",
                                 lipid_data = reactive(all_data$lipid_data_filter),
                                 pattern = "^SSulfate$",
                                 title = input$navbar_selection)

  output$STC_UI <- renderUI({
    req(all_data$lipid_data_filter)

    bubblePlotUI(id = "STC",
                 data = all_data$lipid_data_filter,
                 pattern = "^SSulfate$")
  })

  observe({
    req(filter_STC)

    if(nrow(filter_STC()$filter_data) > 0) {
      all_data$lipid_data_filter <- set_issue_info(lipid_data = isolate(all_data$lipid_data_filter),
                                                   info = isolate(filter_STC()))
    }
  })
  ###

  ### Sterols
  filter_ST <- bubblePlotServer(id = "ST",
                                lipid_data = reactive(all_data$lipid_data_filter),
                                pattern = "^((BR|CA|SI|ST)?[CS]E|Cholesterol|SHex|ST)$",
                                title = input$navbar_selection)

  output$ST_UI <- renderUI({
    req(all_data$lipid_data_filter)

    bubblePlotUI(id = "ST",
                 data = all_data$lipid_data_filter,
                 pattern = "^((BR|CA|SI|ST)?[CS]E|Cholesterol|SHex)$")
  })

  observe({
    req(filter_ST)

    if(nrow(filter_ST()$filter_data) > 0) {
      all_data$lipid_data_filter <- set_issue_info(lipid_data = isolate(all_data$lipid_data_filter),
                                                   info = isolate(filter_ST()))
    }
  })
  ###

  ### Other sterol lipids
  filter_OST <- bubblePlotServer(id = "OST",
                                 lipid_data = reactive(all_data$lipid_data_filter),
                                 pattern = "^AHex(CAS|CS|SIS|BRS|STS)$",
                                 title = input$navbar_selection)

  output$OST_UI <- renderUI({
    req(all_data$lipid_data_filter)

    bubblePlotUI(id = "OST",
                 data = all_data$lipid_data_filter,
                 pattern = "^AHex(CAS|CS|SIS|BRS|STS)$")
  })

  observe({
    req(filter_OST)

    if(nrow(filter_OST()$filter_data) > 0) {
      all_data$lipid_data_filter <- set_issue_info(lipid_data = isolate(all_data$lipid_data_filter),
                                                   info = isolate(filter_OST()))
    }
  })
  ###
  ####

  ### Show the issues
  output$tbl_issues <- renderTable({
    req(all_data$lipid_data_filter)

    all_data$lipid_data_filter %>%
      select(-.data$sample_type) %>%
      pivot_wider(id_cols = .data$my_id:.data$carbon_db,
                  names_from = .data$sample_name,
                  values_from = .data$area) %>%
      filter(.data$keep == FALSE |
               (.data$keep == TRUE & .data$comment == "rename"),
             .data$comment != "remove_class") %>%
      select(.data$my_id:.data$polarity, -.data$scale_DotProduct, -.data$scale_RevDotProduct, .data$keep, .data$comment) %>%
      distinct(.data$my_id,
               .keep_all = TRUE)
  })

  output$tbl_issues_class <- renderTable({
    req(all_data$lipid_data_filter)

    all_data$lipid_data_filter %>%
      select(-.data$sample_type) %>%
      pivot_wider(id_cols = .data$my_id:.data$carbon_db,
                  names_from = .data$sample_name,
                  values_from = .data$area) %>%
      filter(.data$comment == "remove_class") %>%
      select(.data$LipidClass, .data$class_ion, .data$keep, .data$comment) %>%
      distinct(.data$class_ion,
               .keep_all = TRUE)
  })
  #### eind issues part

  #### meta data part
  # get the file
  xlsx_file <- reactive({
    req(input$meta_data_file)
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
    req(all_data$lipid_data_filter,
        all_data$meta_data)

    # make sure this doesn't change if only a new column is selected.
    selected_column <- isolate(input$select_meta_column)
    # nothing should happen if the selected column is 'none'
    if(selected_column != "none") {
      all_data$lipid_data_filter <- merge_data(lipid_data = isolate(all_data$lipid_data_filter),
                                               meta_data = all_data$meta_data(),
                                               by = selected_column)
      all_data$merged_data <- TRUE
    } else {
      all_data$lipid_data_filter <- isolate(all_data$lipid_data_filter)
      all_data$merged_data <- FALSE
    }
  })

  # show the merged data
  output$show_merged_data <- renderDT({
    req(all_data$merged_data)

    if(all_data$merged_data == TRUE) {
      all_data$lipid_data_filter %>%
        select(-.data$MSMSspectrum)
    }
  },
  options = list(pageLength = 10,
                 lengthChange = FALSE,
                 dom = "pt"),
  selection = "none")
  #### end meta merge part

  #### Analysis part
  observe({
    req(all_data$lipid_data_filter,
        all_data$samples_selected)

    # remove samples for the analysis part
    if(!is.null(all_data$samples_selected)) {
      all_data$analysis_data <- isolate(all_data$lipid_data_filter) %>%
        filter(.data$sample_name %in% all_data$samples_selected)
    } else {
      all_data$analysis_data <- isolate(all_data$lipid_data_filter)
    }
  })

  output$compare_samples <- renderPlotly({
    req(all_data$lipid_data_filter,
        input$select_z_heatmap)

    compare_samples_heatmap(lipid_data = all_data$analysis_data,
                            z = input$select_z_heatmap)
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
