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
#' @importFrom dplyr filter mutate select pull distinct case_when relocate bind_rows across
#' @importFrom rlang .data sym !!
#' @importFrom purrr map map_dbl
#' @importFrom magrittr %>%
#' @importFrom tidyr unnest pivot_wider nest
#' @importFrom tidyselect last_col everything matches
#' @importFrom utils head
#' @importFrom tools file_ext
#' @importFrom readxl read_xlsx
#' @importFrom DT renderDT
#' @importFrom plotly renderPlotly plotlyOutput plot_ly add_markers event_data
#' @importFrom shinycssloaders withSpinner
#' @importFrom openxlsx write.xlsx
#'
#' @author Rico Derks

# Define server logic
shinyAppServer <- function(input, output, session) {
  # increase upload limit
  options(shiny.maxRequestSize = 30 * 1024^2)

  rdata_status <- reactiveValues(status = FALSE,
                                 load = TRUE,
                                 comment = "")

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
                             samples_selected = NULL,
                             pca_score_plot = FALSE)

  default_class_ion <- c("ADGGA - [M-H]-", "AHexBRS - [M+HCOO]-", "AHexCAS - [M+HCOO]-", "AHexCS - [M+HCOO]-", "AHexSIS - [M+HCOO]-", "ASM - [M+H]+", "BASulfate - [M-H]-",
                         "BileAcid - [M-H]-", "BMP - [M+NH4]+", "CAR - [M+H]+", "CE - [M+NH4]+", "Cer_ADS - [M+HCOO]-", "Cer_AP - [M+HCOO]-",
                         "Cer_AS - [M+HCOO]-", "Cer_BS - [M+HCOO]-", "Cer_HS - [M+H]+", "Cer_NDS - [M+HCOO]-",
                         "Cer_NP - [M+HCOO]-", "Cer_NS - [M+HCOO]-", "CerP - [M+H]+",
                         "CL - [M+NH4]+", "CoQ - [M+H]+", "DCAE - [M+NH4]+", "DG - [M+NH4]+", "DGGA - [M-H]-", "EtherDG - [M+NH4]+",
                         "EtherLPC - [M+HCOO]-", "EtherLPE - [M-H]-", "EtherMGDG - [M+NH4]+", "EtherPC - [M+HCOO]-",
                         "EtherPE - [M-H]-", "EtherPG - [M-H]-", "EtherPI - [M-H]-", "EtherTG - [M+NH4]+", "FA - [M-H]-", "FAHFA - [M-H]-",
                         "HBMP - [M-H]-", "Hex2Cer - [M+HCOO]-", "HexCer_EOS - [M-H]-", "HexCer_HS - [M+HCOO]-", "HexCer_NS - [M+HCOO]-",
                         "LPA - [M-H]-", "LPC - [M+HCOO]-", "LPE - [M-H]-", "LPI - [M-H]-",
                         "LPS - [M-H]-", "MG - [M+NH4]+", "MGDG - [M+HCOO]-", "MLCL - [M-H]-", "NAE - [M+H]+", "NAGly - [M+H]+", "NAGlySer - [M-H]-",
                         "NAOrn - [M+H]+", "OxFA - [M-H]-", "OxPC - [M+HCOO]-", "OxPE - [M-H]-", "OxPG - [M-H]-", "OxPI - [M-H]-", "OxTG - [M+NH4]+",
                         "PA - [M-H]-", "PC - [M+HCOO]-", "PE - [M-H]-", "PE_Cer - [M-H]-", "PEtOH - [M-H]-",
                         "PG - [M-H]-", "PI - [M-H]-",  "PI_Cer - [M+H]+", "PMeOH - [M-H]-",
                         "PS - [M-H]-", "SHexCer - [M-H]-", "SL - [M-H]-", "SM - [M+H]+", "Sph - [M+H]+",
                         "SQDG - [M-H]-", "SSulfate - [M-H]-", "ST - [M+H-H2O]+", "ST - [M+H]+", "TG - [M+NH4]+", "TG_EST - [M+NH4]+", "VAE - [M+H]+")

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

  #### Read the files ####
  # watch the positive mode file
  observe({
    req(input$res_file_pos,
        input$res_file_neg,
        rdata_status)

    if(rdata_status$status == FALSE) {
      # no data from previous work loaded

      # Initialize the progress bar
      progress <- Progress$new(min = 0,
                               max = 100)
      on.exit(progress$close())

      progress$set(value = 1,
                   message = "Processing...",
                   detail = NULL)

      # when MSDIAl files are loaded do not load RData file
      rdata_status$load = FALSE

      # initialize the tibble for storing all the data
      results <- tibble(filename = c(input$res_file_pos$name, input$res_file_neg$name),
                        datapath = c(input$res_file_pos$datapath, input$res_file_neg$datapath),
                        polarity = c("pos", "neg"))

      # read the data
      results <- results %>%
        filter(.data$datapath != "") %>%
        mutate(raw_data = map(.x = .data$datapath,
                              .f = ~ read_msdial(filename = .x)))

      progress$set(value = 10,
                   message = "Processing...",
                   detail = NULL)

      # cleanup some column names
      results <- clean_up(lipid_data = results)

      progress$set(value = 20,
                   message = "Processing...",
                   detail = NULL)

      # keep only the identified lipids and sort by lipid class, lipid
      all_data$lipid_data <- select_identified(lipid_data = results)

      progress$set(value = 30,
                   message = "Processing...",
                   detail = NULL)

      # make the data long
      all_data$lipid_data_long <- tidy_lipids(lipid_data = all_data$lipid_data)

      progress$set(value = 40,
                   message = "Processing...",
                   detail = NULL)

      ### can't this be simpler??

      # get all lipid classes with their respective ion
      all_data$class_ion <- all_data$lipid_data_long %>%
        distinct(.data$class_ion) %>%
        pull(.data$class_ion)

      # store them
      all_data$class_ion_selected <- all_data$class_ion[all_data$class_ion %in% default_class_ion]

      # get all sample name
      all_data$all_samples <- all_data$lipid_data_long %>%
        distinct(.data$sample_name) %>%
        pull(.data$sample_name)

      all_data$samples_selected <- all_data$all_samples

      # calculate the RSD values
      all_data$qc_results <- calc_rsd(lipid_data = all_data$lipid_data_long)

      progress$set(value = 60,
                   message = "Processing...",
                   detail = NULL)

      # tag lipid class/ion which should be removed
      # find the id's to keep
      keep_class <- all_data$lipid_data %>%
        filter(.data$class_ion %in% default_class_ion) %>%
        distinct(.data$my_id) %>%
        pull(.data$my_id)

      # tag lipids which have a too high RSD value
      # find the lipids to keep
      keep_rsd <- all_data$qc_results %>%
        filter(.data$rsd_area <= 0.3) %>%
        distinct(.data$my_id) %>%
        pull(.data$my_id)

      # tag lipids which have a too low quality
      # find the lipids to keep
      keep_msms <- all_data$lipid_data %>%
        filter(!(.data$DotProduct <= 50 &
                   .data$RevDotProduct <= 50)) %>%  # &
        # .data$keep == TRUE)) %>%
        distinct(.data$my_id) %>%
        pull(.data$my_id)

      progress$set(value = 80,
                   message = "Processing...",
                   detail = NULL)

      all_data$lipid_data_filter <- all_data$lipid_data_long %>%
        mutate(rsd_keep = if_else(.data$my_id %in% keep_rsd,
                                  TRUE,
                                  FALSE),
               match_keep = if_else(.data$my_id %in% keep_msms,
                                    TRUE,
                                    FALSE),
               class_keep = if_else(.data$my_id %in% keep_class,
                                    TRUE,
                                    FALSE),
               keep = if_else(.data$rsd_keep == TRUE & .data$match_keep == TRUE,
                              TRUE,
                              FALSE),
               comment = if_else(.data$rsd_keep == FALSE,
                                 "large_rsd",
                                 if_else(.data$match_keep == FALSE,
                                         "no_match",
                                         "keep")))

      progress$set(value = 100,
                   message = "Processing...",
                   detail = NULL)
    } else {
      # previous work already loaded, load nothing
      rdata_status$comment <- "There is already data loaded from previous work. Please refresh the page to remove everything!!
      Nothing is imported now!!"
    }

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
             p("Lipids are immediately tagged with `no_match`. They will show up in
               the bubble plots (identification tab), but not in the analysis part. Individual
               lipids can be added back via the bubble plots (identification part). Keep
               in mind that when the value of this filter is changed they might be removed again!!")
      )
    )
  })
  ####

  #### Load previously saved data

  observe({
    req(input$load_rdata,
        rdata_status)

    if(rdata_status$load == TRUE) {
      # set the status that previous work is loaded
      rdata_status$status <- TRUE

      # Initialize the progress bar
      progress <- Progress$new(min = 0,
                               max = 100)
      on.exit(progress$close())

      progress$set(value = 50,
                   message = "Processing...",
                   detail = NULL)

      # import the data into a new environment
      import_evn <- load_to_env(RData = input$load_rdata$datapath)

      progress$set(value = 75,
                   message = "Processing...",
                   detail = NULL)

      # load the data into the global environment
      all_data$lipid_data <- import_evn$export$lipid_data
      all_data$lipid_data_long <- import_evn$export$lipid_data_long
      all_data$lipid_data_filter <- import_evn$export$lipid_data_filter
      all_data$clean_data <- import_evn$export$clean_data
      all_data$analysis_data <- import_evn$export$analysis_data
      all_data$merged_data <- import_evn$export$merged_data
      all_data$qc_results <- import_evn$export$qc_results
      all_data$class_ion <- import_evn$export$class_ion
      all_data$class_ion_selected <- import_evn$export$class_ion_selected
      all_data$num_lipid_classes <- import_evn$export$num_lipid_classes
      all_data$all_samples <-  import_evn$export$all_samples
      all_data$samples_selected <- import_evn$export$samples_selected
      all_data$pca_score_plot <- import_evn$export$pca_score_plot

      progress$set(value = 85,
                   message = "Processing...",
                   detail = NULL)

      # load settings
      updateNumericInput(inputId = "rsd_cutoff",
                         label = "RSD cut off value:",
                         value = import_evn$export$input_rsd_cutoff,
                         min = 0,
                         max = 1,
                         step = 0.01)
      updateNumericInput(inputId = "dotprod_cutoff",
                         label = "Dot product cut off value:",
                         value = import_evn$export$input_dotprod_cutoff,
                         min = 0,
                         max = 100,
                         step = 1)
      updateNumericInput(inputId = "revdotprod_cutoff",
                         label = "Reverse dot product cut off value:",
                         value = import_evn$export$input_revdotprod_cutoff,
                         min = 0,
                         max = 100,
                         step = 1)

      # done
      progress$set(value = 100,
                   message = "Processing...",
                   detail = NULL)

    } else {
      rdata_status$comment <- "There are already MSDIAL files loaded. Please refresh the page to remove everything!!
      Nothing is imported now!!"
    }
  })

  output$status_rdata <- renderText({
    req(rdata_status)

    rdata_status$comment
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

    my_col_width <- 3

    tagList(
      p("Select the lipid classes you want to keep."),
      column(width = my_col_width,
             checkboxGroupInput(inputId = "select_PL_class",
                                label = "Glycerophospholipids:",
                                choices = all_data$class_ion[grepl(x = all_data$class_ion, pattern = pattern_PL)],
                                selected = all_data$class_ion_selected[grepl(x = all_data$class_ion_selected, pattern = pattern_PL)])
      ),
      column(width = my_col_width,
             checkboxGroupInput(inputId = "select_Cer_class",
                                label = "Ceramides:",
                                choices = all_data$class_ion[grepl(x = all_data$class_ion, pattern = pattern_Cer)],
                                selected = all_data$class_ion_selected[grepl(x = all_data$class_ion_selected, pattern = pattern_Cer)]),
             checkboxGroupInput(inputId = "select_HexCer_class",
                                label = "Neutral glycosphingolipids:",
                                choices = all_data$class_ion[grepl(x = all_data$class_ion, pattern = pattern_HexCer)],
                                selected = all_data$class_ion_selected[grepl(x = all_data$class_ion_selected, pattern = pattern_HexCer)])
      ),
      column(width = my_col_width,
             checkboxGroupInput(inputId = "select_FA_class",
                                label = "Fatty acyls:",
                                choices = all_data$class_ion[grepl(x = all_data$class_ion, pattern = pattern_FA)],
                                selected = all_data$class_ion_selected[grepl(x = all_data$class_ion_selected, pattern = pattern_FA)]),
             checkboxGroupInput(inputId = "select_PSL_class",
                                label = "Phosphosphingolipids:",
                                choices = all_data$class_ion[grepl(x = all_data$class_ion, pattern = pattern_PSL)],
                                selected = all_data$class_ion_selected[grepl(x = all_data$class_ion_selected, pattern = pattern_PSL)]),
             checkboxGroupInput(inputId = "select_SB_class",
                                label = "Sphingoid bases:",
                                choices = all_data$class_ion[grepl(x = all_data$class_ion, pattern = pattern_SB)],
                                selected = all_data$class_ion_selected[grepl(x = all_data$class_ion_selected, pattern = pattern_SB)]),
             checkboxGroupInput(inputId = "select_SA_class",
                                label = "Acidic glycosphingolipids:",
                                choices = all_data$class_ion[grepl(x = all_data$class_ion, pattern = pattern_SA)],
                                selected = all_data$class_ion_selected[grepl(x = all_data$class_ion_selected, pattern = pattern_SA)]),
             checkboxGroupInput(inputId = "select_GL_class",
                                label = "Glcyerolipids:",
                                choices = all_data$class_ion[grepl(x = all_data$class_ion, pattern = pattern_GL)],
                                selected = all_data$class_ion_selected[grepl(x = all_data$class_ion_selected, pattern = pattern_GL)]),
             checkboxGroupInput(inputId = "select_CL_class",
                                label = "Cardiolipins:",
                                choices = all_data$class_ion[grepl(x = all_data$class_ion, pattern = pattern_CL)],
                                selected = all_data$class_ion_selected[grepl(x = all_data$class_ion_selected, pattern = pattern_CL)])
      ),
      column(width = my_col_width,
             checkboxGroupInput(inputId = "select_STL_class",
                                label = "Sterol lipids:",
                                choices = all_data$class_ion[grepl(x = all_data$class_ion, pattern = pattern_STL)],
                                selected = all_data$class_ion_selected[grepl(x = all_data$class_ion_selected, pattern = pattern_STL)]),
             checkboxGroupInput(inputId = "select_ACPIM_class",
                                label = "Glycerophosphoinositolglycans:",
                                choices = all_data$class_ion[grepl(x = all_data$class_ion, pattern = pattern_ACPIM)],
                                selected = all_data$class_ion_selected[grepl(x = all_data$class_ion_selected, pattern = pattern_ACPIM)]),
             checkboxGroupInput(inputId = "select_PRL_class",
                                label = "Prenol lipids:",
                                choices = all_data$class_ion[grepl(x = all_data$class_ion, pattern = pattern_PRL)],
                                selected = all_data$class_ion_selected[grepl(x = all_data$class_ion_selected, pattern = pattern_PRL)])
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

    cor_heatmap(lipid_data = all_data$lipid_data)
  })

  # create UI for correlation plot
  output$corplot <- renderUI({
    req(all_data$lipid_data_long)

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
  # observe which sample needs to be selected
  observeEvent(input$select_samples, {
    req(input$select_samples)

    # store which samples are selected
    all_data$samples_selected <- input$select_samples
  })

  # observe if RSD filtering is changed
  observeEvent(input$rsd_cutoff, {
    req(all_data$lipid_data_filter,
        all_data$qc_results)

    tmp_filter <- isolate(all_data$lipid_data_filter)

    # which lipids have a low RSD
    if(is.null(input$rsd_cutoff)) {
      rsd_cutoff <- 0.3
    } else {
      rsd_cutoff <- input$rsd_cutoff
    }

    keep_lipids_rsd <- all_data$qc_results %>%
      filter(.data$rsd_area <= rsd_cutoff) %>%
      distinct(.data$my_id) %>%
      pull(.data$my_id)

    all_data$lipid_data_filter <- tmp_filter %>%
      mutate(rsd_keep = if_else(.data$my_id %in% keep_lipids_rsd,
                                TRUE,
                                FALSE),
             comment = if_else(.data$my_id %in% keep_lipids_rsd,
                               "keep",
                               "large_rsd"),
             keep = if_else(.data$rsd_keep == TRUE &
                              .data$match_keep == TRUE &
                              .data$rt_keep == TRUE,
                            TRUE,
                            FALSE))
  })

  # observe if product/dotproduct filtering is chagned
  observeEvent({
    input$dotprod_cutoff
    input$revdotprod_cutoff
  }, {
    req(all_data$lipid_data_filter)

    tmp_filter <- isolate(all_data$lipid_data_filter)

    if(is.null(input$dotprod_cutoff) |
       is.null(input$revdotprod_cutoff)) {
      dotprod_cutoff <- 50
      revdotprod_cutoff <- 50
    } else {
      dotprod_cutoff <- input$dotprod_cutoff
      revdotprod_cutoff <- input$revdotprod_cutoff
    }

    # which lipids have a high dotproduct and revdotproduct
    keep_lipids_msms <- tmp_filter %>%
      filter(!(.data$DotProduct <= dotprod_cutoff &
                 .data$RevDotProduct <= revdotprod_cutoff)) %>%
      distinct(.data$my_id) %>%
      pull(.data$my_id)

    all_data$lipid_data_filter <- tmp_filter %>%
      mutate(match_keep = if_else(.data$my_id %in% keep_lipids_msms,
                                  TRUE,
                                  FALSE),
             comment = if_else(.data$my_id %in% keep_lipids_msms,
                               "keep",
                               "no_match"),
             keep = if_else(.data$rsd_keep == TRUE &
                              .data$match_keep == TRUE &
                              .data$rt_keep == TRUE,
                            TRUE,
                            FALSE))
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
  }, {
    req(all_data$lipid_data_filter)

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

    # get the id's to keep lipids which lipid class is selected
    keep_lipids_class <- tmp_filter %>%
      filter(.data$class_ion %in% all_data$class_ion_selected) %>%
      distinct(.data$my_id) %>%
      pull(.data$my_id)

    all_data$lipid_data_filter <- tmp_filter %>%
      mutate(class_keep = if_else(.data$my_id %in% keep_lipids_class,
                                  TRUE,
                                  FALSE))
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
      all_data$lipid_data_filter <- set_issue_info(lipid_data = all_data$lipid_data_filter,
                                                   info = filter_FA())
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
      all_data$lipid_data_filter <- set_issue_info(lipid_data = all_data$lipid_data_filter,
                                                   info = filter_FAM())
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
      all_data$lipid_data_filter <- set_issue_info(lipid_data = all_data$lipid_data_filter,
                                                   info = filter_FE())
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
      all_data$lipid_data_filter <- set_issue_info(lipid_data = all_data$lipid_data_filter,
                                                   info = filter_EGL())
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
      all_data$lipid_data_filter <- set_issue_info(lipid_data = all_data$lipid_data_filter,
                                                   info = filter_GL())
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
      all_data$lipid_data_filter <- set_issue_info(lipid_data = all_data$lipid_data_filter,
                                                   info = filter_GLDG())
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
      all_data$lipid_data_filter <- set_issue_info(lipid_data = all_data$lipid_data_filter,
                                                   info = filter_OGL())
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
      all_data$lipid_data_filter <- set_issue_info(lipid_data = all_data$lipid_data_filter,
                                                   info = filter_PA())
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
      all_data$lipid_data_filter <- set_issue_info(lipid_data = all_data$lipid_data_filter,
                                                   info = filter_PC())
    }
  })
  ###

  ### Glycerophospho ethanolamines
  filter_PE <- bubblePlotServer(id = "PE",
                                lipid_data = reactive(all_data$lipid_data_filter),
                                pattern = "^(LNA)?(Ether)?L?PE(\\(P\\))?$",
                                title = input$navbar_selection)

  output$PE_UI <- renderUI({
    req(all_data$lipid_data_filter)

    bubblePlotUI(id = "PE",
                 data = all_data$lipid_data_filter,
                 pattern = "^(LNA)?(Ether)?L?PE(\\(P\\))?$")
  })

  observe({
    req(filter_PE)

    if(nrow(filter_PE()$filter_data) > 0) {
      all_data$lipid_data_filter <- set_issue_info(lipid_data = all_data$lipid_data_filter,
                                                   info = filter_PE())
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
      all_data$lipid_data_filter <- set_issue_info(lipid_data = all_data$lipid_data_filter,
                                                   info = filter_PG())
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
      all_data$lipid_data_filter <- set_issue_info(lipid_data = all_data$lipid_data_filter,
                                                   info = filter_CL())
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
      all_data$lipid_data_filter <- set_issue_info(lipid_data = all_data$lipid_data_filter,
                                                   info = filter_AcPIM())
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
      all_data$lipid_data_filter <- set_issue_info(lipid_data = all_data$lipid_data_filter,
                                                   info = filter_PI())
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
      all_data$lipid_data_filter <- set_issue_info(lipid_data = all_data$lipid_data_filter,
                                                   info = filter_PS())
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
      all_data$lipid_data_filter <- set_issue_info(lipid_data = all_data$lipid_data_filter,
                                                   info = filter_OPL())
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
      all_data$lipid_data_filter <- set_issue_info(lipid_data = all_data$lipid_data_filter,
                                                   info = filter_OGPL())
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
      all_data$lipid_data_filter <- set_issue_info(lipid_data = all_data$lipid_data_filter,
                                                   info = filter_PRL())
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
      all_data$lipid_data_filter <- set_issue_info(lipid_data = all_data$lipid_data_filter,
                                                   info = filter_AcGL())
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
      all_data$lipid_data_filter <- set_issue_info(lipid_data = all_data$lipid_data_filter,
                                                   info = filter_Cer())
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
      all_data$lipid_data_filter <- set_issue_info(lipid_data = all_data$lipid_data_filter,
                                                   info = filter_PSL())
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
      all_data$lipid_data_filter <- set_issue_info(lipid_data = all_data$lipid_data_filter,
                                                   info = filter_NPSL())
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
      all_data$lipid_data_filter <- set_issue_info(lipid_data = all_data$lipid_data_filter,
                                                   info = filter_SB())
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
      all_data$lipid_data_filter <- set_issue_info(lipid_data = all_data$lipid_data_filter,
                                                   info = filter_BA())
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
      all_data$lipid_data_filter <- set_issue_info(lipid_data = all_data$lipid_data_filter,
                                                   info = filter_SC())
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
      all_data$lipid_data_filter <- set_issue_info(lipid_data = all_data$lipid_data_filter,
                                                   info = filter_STC())
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
      all_data$lipid_data_filter <- set_issue_info(lipid_data = all_data$lipid_data_filter,
                                                   info = filter_ST())
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
      all_data$lipid_data_filter <- set_issue_info(lipid_data = all_data$lipid_data_filter,
                                                   info = filter_OST())
    }
  })
  ###
  ####

  ### Show the issues
  output$tbl_issues <- renderDT({
    req(all_data$lipid_data_filter)

    all_data$lipid_data_filter %>%
      select(-.data$sample_type) %>%
      pivot_wider(id_cols = .data$my_id:.data$carbon_db,
                  names_from = .data$sample_name,
                  values_from = .data$area) %>%
      filter(.data$keep == FALSE,
             .data$class_keep == TRUE) %>%
      select(.data$my_id:.data$polarity, -.data$scale_DotProduct, -.data$scale_RevDotProduct, .data$comment, .data$keep, .data$rsd_keep, .data$match_keep, .data$rt_keep) %>%
      distinct(.data$my_id,
               .keep_all = TRUE)
  },
  options = list(pageLength = 10,
                 lengthChange = FALSE,
                 dom = "pt",
                 ordering = TRUE),
  selection = "none",
  rownames = FALSE)

  output$tbl_issues_class <- renderDT({
    req(all_data$lipid_data_filter)

    all_data$lipid_data_filter %>%
      select(-.data$sample_type) %>%
      pivot_wider(id_cols = .data$my_id:.data$carbon_db,
                  names_from = .data$sample_name,
                  values_from = .data$area) %>%
      filter(.data$class_keep == FALSE) %>%
      select(.data$LipidClass, .data$class_ion, .data$class_keep) %>%
      distinct(.data$class_ion,
               .keep_all = TRUE)
  },
  options = list(pageLength = 10,
                 lengthChange = FALSE,
                 dom = "pt",
                 ordering = TRUE),
  selection = "none",
  rownames = FALSE)
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
  selection = "none",
  rownames = FALSE)

  # update on which column to merge
  observe({
    req(all_data$meta_data)

    # get the column names of the meta data
    merge_colnames <- colnames(all_data$meta_data())

    # update the select input
    updateSelectInput(session = session,
                      inputId = "select_meta_column",
                      label = "Select column for merging:",
                      choices = c("none", merge_colnames),
                      selected = "none")
  })

  output$select_group_column_ui <- renderUI({
    req(all_data$meta_data)

    if(all_data$merged_data == TRUE) {
      # get the column names of the meta data
      all_colnames <- colnames(all_data$meta_data())
      all_colnames <- all_colnames[!(all_colnames %in% input$select_meta_column)]

      tagList(
        checkboxGroupInput(inputId = "select_group_column",
                           label = "Select column to be used for grouping:",
                           choices = all_colnames,
                           selected = NULL)
      )
    }
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
  selection = "none",
  rownames = FALSE)
  #### end meta merge part

  #### Analysis part

  observe({
    req(all_data$lipid_data_filter)

    # remove samples for the analysis part
    if(!is.null(all_data$samples_selected)) {
      all_data$analysis_data <- isolate(all_data$lipid_data_filter) %>%
        filter(.data$sample_name %in% all_data$samples_selected)
    } else {
      all_data$analysis_data <- isolate(all_data$lipid_data_filter)
    }
  })

  #### heatmap
  # update color selection
  observeEvent(input$select_group_column, {
    req(input$select_group_column)

    if(!is.null(input$select_group_column)) {
      updateSelectInput(session = session,
                        inputId = "select_heatmap_group",
                        label = "Group color:",
                        choices = c("none", input$select_group_column),
                        selected = "none")
    }
  })

  output$compare_samples <- renderPlotly({
    req(all_data$lipid_data_filter,
        input$select_z_heatmap)

    # no merge
    if(input$select_heatmap_group == "none") {
      compare_samples_heatmap(lipid_data = all_data$analysis_data,
                              cent_scale = input$heatmap_zscore,
                              z = input$select_z_heatmap,
                              clust = input$heatmap_use_clust)
    } else {
      compare_samples_heatmap(lipid_data = all_data$analysis_data,
                              cent_scale = input$heatmap_zscore,
                              z = input$select_z_heatmap,
                              clust = input$heatmap_use_clust,
                              sample_group = input$select_heatmap_group)
    }
  })
  #### end heatmap

  #### compare samples
  test_result <- reactive({
    req(all_data$analysis_data,
        input$test_group1,
        input$test_group2,
        input$select_test,
        input$select_test_normalization,
        input$select_test_transformation)

    results_test <- do_stat_test(lipid_data = isolate(all_data$analysis_data),
                                 group = input$test_select_group,
                                 group1_name = input$test_group1,
                                 group2_name = input$test_group2,
                                 normalization = input$select_test_normalization,
                                 transformation = input$select_test_transformation,
                                 test = input$select_test)

    return(results_test)
  })

  # create some ui output
  output$test_group_selection <- renderUI({
    req(all_data$meta_data)

    if(all_data$merged_data == TRUE & !is.null(input$select_group_column)) {
      tagList(
        selectInput(inputId = "test_select_group",
                    label = "Select a group:",
                    choices = input$select_group_column,
                    selected = input$select_group_column[1]),
        uiOutput(outputId = "test_vs_groups")
      )
    }
  })

  output$test_vs_groups <- renderUI({
    tagList(
      selectInput(inputId = "test_group1",
                  label = "Group 1:",
                  choices = "none"),
      HTML("<center>vs</center>"),
      selectInput(inputId = "test_group2",
                  label = "Group 2:",
                  choices = "none")
    )
  })

  observeEvent(input$test_select_group, {
    req(all_data$meta_data)

    if(input$test_select_group != "none") {
      # get the groups
      group_options <- all_data$meta_data() %>%
        mutate(across(everything(), as.character)) %>%
        select(matches(paste0("^", input$test_select_group, "$"))) %>%
        pull() %>%
        unique()

      # remove NA
      group_options <- group_options[!is.na(group_options)]

      updateSelectInput(inputId = "test_group1",
                        label = "Group 1:",
                        choices = group_options,
                        selected = group_options[1])

      updateSelectInput(inputId = "test_group2",
                        label = "Group 2:",
                        choices = group_options,
                        selected = group_options[2])
    }
  })

  output$volcano_plot <- renderPlotly({
    req(test_result)

    if(!is.null(test_result())) {
      volcano_plot(lipid_data = test_result(),
                   pvalue_adjust = input$test_cor_pvalue,
                   title = paste0(input$test_group1, " vs ", input$test_group2))
    }
  })

  output$test_boxplot <- renderPlotly({
    req(test_result)

    if(!is.null(test_result())) {
      # capture the click event
      # this contains a column with the shortlipidname (column name: customdata)
      my_data <- event_data(event = "plotly_click",
                            source = "volcano_plot_click")

      if(!is.null(my_data)) {
        # restructure data
        plot_data <- test_result() %>%
          filter(.data$ShortLipidName %in% my_data$customdata) %>%
          select(.data$test_data) %>%
          unnest(.data$test_data)

        # show the boxplot
        box_plot(lipid_data = plot_data,
                 title = paste0("Lipid: ", my_data$customdata))
      }
    }


  })

  #### end compare samples

  #### PCA
  # do the PCA analysis
  pca_data <- reactive({
    req(all_data$analysis_data,
        input$select_pca_observations,
        input$select_pca_normalization,
        input$select_pca_scaling,
        input$select_pca_transformation)

    data_pca <- do_pca(lipid_data = isolate(all_data$analysis_data),
                       observations = input$select_pca_observations,
                       normalization = input$select_pca_normalization,
                       scaling = input$select_pca_scaling,
                       transformation = input$select_pca_transformation)

    return(data_pca)
  })

  # show the scores plot
  output$pca_scores_plot <- renderPlotly({
    req(pca_data,
        input$select_pca_scores_x,
        input$select_pca_scores_y,
        input$select_pca_scores_color)

    # check if there was a merge
    if(all_data$merged_data == TRUE & !is.null(input$select_group_column)) {
      # if so merge als with scores data
      scores_data <- pca_data()$scores %>%
        left_join(y = all_data$meta_data(),
                  by = c("sample_name" = isolate(input$select_meta_column)),
                  suffix = c("", ".y"))
    } else {
      # no merge
      scores_data <- pca_data()$scores
    }

    pca_scores_plot(scores_data = scores_data,
                    xaxis = input$select_pca_scores_x,
                    yaxis = input$select_pca_scores_y,
                    color_by = input$select_pca_scores_color)
  })

  # show the loadings plot
  output$pca_loadings_plot <- renderPlotly({
    req(pca_data,
        input$select_pca_loadings_x,
        input$select_pca_loadings_y)

    pca_loadings_plot(loadings_data = pca_data()$loadings,
                      xaxis = input$select_pca_loadings_x,
                      yaxis = input$select_pca_loadings_y)
  })

  # show the explained variance
  output$pca_explained_var <- renderPlotly({
    req(pca_data)
    # input$select_num_components)

    pca_explained_var_plot(exp_var_data = pca_data()$explained_var)
    # input$select_num_components)
  })

  output$pca_data <- renderPrint({
    req(pca_data)

    pca_data()
  })

  # update color selection
  observeEvent(input$select_group_column, {
    req(input$select_group_column)

    if(!is.null(input$select_group_column)) {
      updateSelectInput(session = session,
                        inputId = "select_pca_scores_color",
                        label = "Color observations by:",
                        choices = c("none", input$select_group_column),
                        selected = "none")
    }
  })

  output$pca_plot_ui <- renderUI({
    req(pca_data)

    tagList(
      splitLayout(cellWidths = c("20%", "40%", "40%"),
                  withSpinner(plotlyOutput(outputId = "pca_explained_var"),
                              type = 5),
                  withSpinner(plotlyOutput(outputId = "pca_scores_plot"),
                              type = 5),
                  withSpinner(plotlyOutput(outputId = "pca_loadings_plot"),
                              type = 5))
    )
  })

  # create variable plot
  output$pca_var_plot <- renderPlotly({
    req(pca_data)

    # capture the click event
    # this contains a column with the sample names (column name: customdata)
    my_data <- event_data(event = "plotly_click",
                          source = "pca_scores_plot")

    if(!is.null(my_data)) {
      # restructure data
      plot_data <- pca_data()$preprocess_data %>%
        filter(.data$sample_name %in% my_data$customdata)

      # make the plot
      pca_variable_plot(var_data = plot_data,
                        sample_name = my_data$customdata)
    }
  })

  # create observation plot
  output$pca_obs_plot <- renderPlotly({
    req(pca_data,
        input$select_pca_scores_color)

    # capture the click event
    # this contains a column with the sample names (column name: customdata)
    my_data <- event_data(event = "plotly_click",
                          source = "pca_loadings_plot")

    if(!is.null(my_data)) {
      # check if there was a merge
      if(all_data$merged_data == TRUE & !is.null(input$select_group_column)) {
        # if so merge als with scores data
        plot_data <- pca_data()$preprocess_data %>%
          filter(.data$ShortLipidName %in% my_data$customdata) %>%
          left_join(y = all_data$meta_data(),
                    by = c("sample_name" = isolate(input$select_meta_column)),
                    suffix = c("", ".y"))

        # make the plot
        pca_observation_plot(obs_data = plot_data,
                             var_name = my_data$customdata,
                             color_by = input$select_pca_scores_color)
      } else {
        # no merge
        plot_data <- pca_data()$preprocess_data %>%
          filter(.data$ShortLipidName %in% my_data$customdata)

        # make the plot
        pca_observation_plot(obs_data = plot_data,
                             var_name = my_data$customdata)
      }
    }
  })
  ####

  #### end analysis part

  #### Export
  output$download_lipid_xlsx <- downloadHandler(
    filename = function() {
      # create a filename
      paste("Lipid_list_", Sys.Date(), ".xlsx", sep = "")
    },
    content = function(file) {
      req(all_data$analysis_data)
      # export needs to be in wide format
      export_wide <- all_data$analysis_data %>%
        filter(.data$sample_type != "blank",
               .data$keep == TRUE,
               .data$class_keep == TRUE) %>%
        pivot_wider(id_cols = c(.data$my_id, .data$LongLipidName, .data$ShortLipidName, .data$LipidClass),
                    names_from = .data$sample_name,
                    values_from = .data$area) %>%
        mutate(across(everything(), as.character))

      if(all_data$merged_data == TRUE) {
        t_meta <- all_data$analysis_data %>%
          # remove blanks
          filter(.data$sample_type != "blank") %>%
          # select the columns
          select(.data$sample_name, .data$sample_type:last_col()) %>%
          select(-matches("\\.y")) %>%
          # sort by filename
          arrange(.data$sample_name) %>%
          distinct(.data$sample_name,
                   .keep_all = TRUE) %>%
          # everything needs to be character in order to transpose
          mutate(across(everything(), as.character)) %>%
          pivot_longer(-.data$sample_name) %>%
          pivot_wider(names_from = .data$sample_name,
                      values_from = .data$value) %>%
          rename(my_id = .data$name) %>%
          mutate(LongLipidName = NA_character_,
                 ShortLipidName = NA_character_,
                 LipidClass = NA_character_)

        export_wide <- bind_rows(t_meta, export_wide) %>%
          #  sort the columns in the correct order
          relocate(c(.data$LipidClass, .data$ShortLipidName, .data$LongLipidName), .after = .data$my_id)
      }

      write.xlsx(x = export_wide,
                 file = file)
    }
  )

  # save the current state of your work
  output$save_rdata <- downloadHandler(
    filename = function() {
      # create a filename
      paste("Current_state_", Sys.Date(), ".Rdata", sep = "")
    },
    content = function(file) {
      # create object to be saved
      # Initialize list
      export <- list()
      # export the data
      export$lipid_data <- all_data$lipid_data
      export$lipid_data_long <- all_data$lipid_data_long
      export$lipid_data_filter <- all_data$lipid_data_filter
      export$clean_data <- all_data$clean_data
      export$analysis_data <- all_data$analysis_data
      export$merged_data <- all_data$merged_data
      export$qc_results <- all_data$qc_results
      export$class_ion <- all_data$class_ion
      export$class_ion_selected <- all_data$class_ion_selected
      export$num_lipid_classes <- all_data$num_lipid_classes
      export$all_samples <- all_data$all_samples
      export$samples_selected <- all_data$samples_selected
      export$pca_score_plot <- all_data$pca_score_plot
      # export the settings
      # filter settings
      export$input_rsd_cutoff <- input$rsd_cutoff
      export$input_dotprod_cutoff <- input$dotprod_cutoff
      export$input_revdotprod_cutoff <- input$revdotprod_cutoff
      # lipid class selection
      export$input_select_PL_class <- input$select_PL_class
      export$input_select_Cer_class <- input$select_Cer_class
      export$input_select_HexCer_class <- input$select_HexCer_class
      export$input_select_FA_class <- input$select_FA_class
      export$input_select_PSL_class <- input$select_PSL_class
      export$input_select_SB_class <- input$select_SB_class
      export$input_select_SA_class <- input$select_SA_class
      export$input_select_GL_class <- input$select_GL_class
      export$input_select_CL_class <- input$select_CL_class
      export$input_select_STL_class <- input$select_STL_class
      export$input_select_ACPIM_class <- input$select_ACPIM_class
      export$input_select_PRL_class <- input$select_PRL_class
      # sample selection
      export$input_select_samples <- input$select_samples

      # save the object
      save(export,
           file = file)
    }
  )

  #### End Export

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
