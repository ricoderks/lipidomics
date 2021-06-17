#' @title Shiny app server object
#'
#' @description This is the UI function for the shiny app.
#'
#' @import shiny
#' @importFrom shinyjs hidden
#' @importFrom DT DTOutput
#' @importFrom plotly plotlyOutput
#' @importFrom shinycssloaders withSpinner

# create the shiny application user interface
shinyAppUI <- fluidPage(
  shinyjs::useShinyjs(),

  navbarPage(title = "CPM - Lipidomics",
             id = "navbar_selection",
             # tabPanel Files
             tabPanel(title = "Files",
                      fluidPage(
                        fluidRow(
                          column(width = 12,
                                 splitLayout(cellWidths = c("35px", NA),
                                             actionButton(inputId = "btn_info_msdial",
                                                          label = "",
                                                          icon = icon("info")),
                                             h4("MS-DIAL files")))
                        ),
                        fluidRow(
                          shinyjs::hidden(uiOutput(outputId = "info_msdial")),
                          column(width = 9,
                                 fileInput(inputId = "res_file_pos",
                                           label = "Positive mode:",
                                           multiple = FALSE,
                                           accept = c(".txt"),
                                           width = 400),
                                 fileInput(inputId = "res_file_neg",
                                           label = "Negative mode:",
                                           multiple = FALSE,
                                           accept = c(".txt"),
                                           width = 400)),
                          style = "background-color: #E8E8E8"
                        ),
                        # create some empty space
                        fluidRow(column = 12,
                                 p("")),
                        fluidRow(
                          column(width = 12,
                                 splitLayout(cellWidths = c("35px", NA),
                                             actionButton(inputId = "btn_info_meta",
                                                          label = "",
                                                          icon = icon("info")),
                                             h4("Meta data")))
                        ),
                        fluidRow(
                          shinyjs::hidden(uiOutput(outputId = "info_meta")),
                          column(width = 5,
                                 p("Read an Excel file with meta data."),
                                 fileInput(inputId = "meta_data_file",
                                           label = "Meta data file:",
                                           multiple = FALSE,
                                           accept = c(".xlsx"),
                                           width = 400),
                                 selectInput(inputId = "select_meta_column",
                                             label = "Select column for merging:",
                                             choices = "none",
                                             selected = "none"),
                                 # show status on which column the merge was done
                                 htmlOutput(outputId = "status_merge"),
                                 # the merge button
                                 actionButton(inputId = "btn_merge_meta",
                                              label = "Merge")),
                          column(width = 4,
                                 # checkboxGroupInput(inputId = "select_group_column",
                                 #              label = "Select column to be used for grouping:",
                                 #              choices = NULL,
                                 #              selected = NULL)
                                 uiOutput(outputId = "select_group_column_ui")
                          ),
                          style = "background-color: #E8E8E8"
                        )
                      )
             ), # end tabPanel Files
             # start navbarMenu filter
             navbarMenu(title = "Filter",
                        # tabPanel settings
                        tabPanel(title = "Settings",
                                 fluidPage(
                                   fluidRow(
                                     column(width = 12,
                                            splitLayout(cellWidths = c("35px", NA),
                                                        actionButton(inputId = "btn_info_qcfilter",
                                                                     label = "",
                                                                     icon = icon("info")),
                                                        h4("QC")))
                                   ),
                                   fluidRow(
                                     shinyjs::hidden(uiOutput(outputId = "info_qcfilter")),
                                     column(width = 9,
                                            numericInput(inputId = "rsd_cutoff",
                                                         label = "RSD cut off value:",
                                                         value = 0.3,
                                                         min = 0,
                                                         max = 1,
                                                         step = 0.01,
                                                         width = "150px"),
                                            style = "background-color: #E8E8E8")
                                   ),
                                   # create some empty space
                                   fluidRow(column = 12,
                                            p("")),
                                   fluidRow(
                                     column(width = 12,
                                            splitLayout(cellWidths = c("35px", NA),
                                                        actionButton(inputId = "btn_info_prodfilter",
                                                                     label = "",
                                                                     icon = icon("info")),
                                                        h4("Identification")))
                                   ),
                                   fluidRow(
                                     shinyjs::hidden(uiOutput(outputId = "info_prodfilter")),
                                     column(width = 9,
                                            splitLayout(
                                              cellWidths = c("200px", NA),
                                              numericInput(inputId = "dotprod_cutoff",
                                                           label = "Dot product cut off value:",
                                                           value = 50,
                                                           min = 0,
                                                           max = 100,
                                                           step = 1,
                                                           width = "150px"),
                                              numericInput(inputId = "revdotprod_cutoff",
                                                           label = "Reverse dot product cut off value:",
                                                           value = 50,
                                                           min = 0,
                                                           max = 100,
                                                           step = 1)),
                                            style = "background-color: #E8E8E8")
                                   )
                                 )
                        ), # end of tabpanel settings
                        # tabPanel lipid classes
                        tabPanel(title = "Lipid classes",
                                 fluidPage(
                                   fluidRow(column = 12,
                                            h4("Lipid classes")),
                                   fluidRow(column = 12,
                                            uiOutput(outputId = "select_lipid_classes"),
                                            style = "background-color: #E8E8E8"
                                   ))
                        ), # end of tabpanel lipid classes
                        # tabPanel samples
                        tabPanel(title = "Samples",
                                 fluidPage(
                                   fluidRow(column = 12,
                                            uiOutput(outputId = "samples_list"),
                                            style = "background-color: #E8E8E8"))
                        ) # end of tabpanel samples
             ), # end navbarMenu filter
             # start navbarMenu data
             navbarMenu(title = "Data",
                        # tabpanel Lipid data
                        tabPanel(title = "Lipid data",
                                 fluidPage(
                                   fluidRow(column = 12,
                                            shinycssloaders::withSpinner(DT::DTOutput(outputId = "lipid_data_table"),
                                                                         type = 5))
                                 )
                        ), # end tabPanel Data
                        # start tabpanel meta data
                        tabPanel(title = "Meta data",
                                 fluidPage(
                                   fluidRow(
                                     column(width = 12,
                                            DT::DTOutput(outputId = "show_meta_data"))
                                   ))
                        ), # end of tabpanel meta data
                        # start tabpanel merged data
                        tabPanel(title = "Merged data",
                                 fluidPage(
                                   fluidRow(column = 12,
                                            h4("Merged data"),
                                            p("This is only to have a quick overview of the merged data. Merged data is in long format."),
                                            DT::DTOutput(outputId = "show_merged_data")
                                   )
                                 )
                        ) # end tabpanel merged data
             ), # end navbarMenu data
             # navbarMenu QC
             navbarMenu(title = "QC",
                        # tabpanel QC overall
                        tabPanel(title = "QC - overall",
                                 fluidPage(
                                   fluidRow(column = 12,
                                            h4("Histogram"),
                                            p("Histogram showing the RSD vales for all lipids over all QCpool samples."),
                                            plotOutput(outputId = "rsd_all",
                                                       width = "50%")
                                   )
                                 )
                        ), # end tabpanel QC overall
                        #tabpanel QC lipid class
                        tabPanel(title = "QC - Lipid class",
                                 fluidPage(
                                   fluidRow(column = 12,
                                            shinycssloaders::withSpinner(uiOutput(outputId = "rsd_lipidclass_ui"),
                                                                         type = 5)
                                   )
                                 )
                        ), # end tabpanel QC lipid class
                        #tabpanel correlation plot
                        tabPanel(title = "QC - Correlation",
                                 fluidPage(
                                   fluidRow(column = 12,
                                            shinycssloaders::withSpinner(uiOutput(outputId = "corplot"),
                                                                         type = 5)
                                   )
                                 )
                        ) # end tabpanel correlation plot
             ), # end navbarMenu QC
             # navbarMenu identification
             navbarMenu(title = "Identification",
                        "Fatty acyls",
                        tabPanel(title = "Fatty acids and conjugates",
                                 # tableOutput(outputId = "debug"),
                                 shinycssloaders::withSpinner(uiOutput(outputId = "FA_UI"),
                                                              type = 5)
                        ),
                        tabPanel(title = "Fatty amides",
                                 shinycssloaders::withSpinner(uiOutput(outputId = "FAM_UI"),
                                                              type = 5)
                        ),
                        tabPanel(title = "Fatty esters",
                                 shinycssloaders::withSpinner(uiOutput(outputId = "FE_UI"),
                                                              type = 5)
                        ),
                        "----",
                        "Glyceroplipids",
                        tabPanel(title = "Ether/Oxidized glycerolipids",
                                 shinycssloaders::withSpinner(uiOutput(outputId = "EGL_UI"),
                                                              type = 5)
                        ),
                        tabPanel(title = "Glycerolipids",
                                 shinycssloaders::withSpinner(uiOutput(outputId = "GL_UI"),
                                                              type = 5)
                        ),
                        tabPanel(title = "Glycosyldiradylglycerols ",
                                 shinycssloaders::withSpinner(uiOutput(outputId = "GLDG_UI"),
                                                              type = 5)
                        ),
                        tabPanel(title = "Other glycerolipids ",
                                 shinycssloaders::withSpinner(uiOutput(outputId = "OGL_UI"),
                                                              type = 5)
                        ),
                        "----",
                        "Glycerophospholipids",
                        tabPanel(title = "Glycerophosphates (PA)",
                                 shinycssloaders::withSpinner(uiOutput(outputId = "PA_UI"),
                                                              type = 5)
                        ),
                        tabPanel(title = "Glycerophosphocholines (PC)",
                                 shinycssloaders::withSpinner(uiOutput(outputId = "PC_UI"),
                                                              type = 5)
                        ),
                        tabPanel(title = "Glycerophosphoethanolamines (PE)",
                                 shinycssloaders::withSpinner(uiOutput(outputId = "PE_UI"),
                                                              type = 5)
                        ),
                        tabPanel(title = "Glycerophosphoglycerols (PG)",
                                 shinycssloaders::withSpinner(uiOutput(outputId = "PG_UI"),
                                                              type = 5)
                        ),
                        tabPanel(title = "Glycerophosphoglycerophosphoglycerols (CL)",
                                 shinycssloaders::withSpinner(uiOutput(outputId = "CL_UI"),
                                                              type = 5)
                        ),
                        tabPanel(title = "Glycerophosphoinositolglycans",
                                 shinycssloaders::withSpinner(uiOutput(outputId = "AcPIM_UI"),
                                                              type = 5)
                        ),
                        tabPanel(title = "Glycerophosphoinositols (PI)",
                                 shinycssloaders::withSpinner(uiOutput(outputId = "PI_UI"),
                                                              type = 5)
                        ),
                        tabPanel(title = "Glycerophosphoserines (PS)",
                                 shinycssloaders::withSpinner(uiOutput(outputId = "PS_UI"),
                                                              type = 5)
                        ),
                        tabPanel(title = "Other glycerophospholipids",
                                 shinycssloaders::withSpinner(uiOutput(outputId = "OGPL_UI"),
                                                              type = 5)
                        ),
                        tabPanel(title = "Oxidized glycerophospholipids",
                                 shinycssloaders::withSpinner(uiOutput(outputId = "OPL_UI"),
                                                              type = 5)
                        ),
                        "----",
                        tabPanel(title = "Prenol lipids",
                                 shinycssloaders::withSpinner(uiOutput(outputId = "PRL_UI"),
                                                              type = 5)
                        ),
                        "----",
                        "Sphingolipids",
                        tabPanel(title = "Acidic glycosphingolipids",
                                 shinycssloaders::withSpinner(uiOutput(outputId = "AcGL_UI"),
                                                              type = 5)
                        ),
                        tabPanel(title = "Ceramides ",
                                 shinycssloaders::withSpinner(uiOutput(outputId = "Cer_UI"),
                                                              type = 5)
                        ),
                        tabPanel(title = "Phosphosphingolipids ",
                                 shinycssloaders::withSpinner(uiOutput(outputId = "PSL_UI"),
                                                              type = 5)
                        ),
                        tabPanel(title = "Neutral glycosphingolipids",
                                 shinycssloaders::withSpinner(uiOutput(outputId = "NPSL_UI"),
                                                              type = 5)
                        ),
                        tabPanel(title = "Sphingoid bases",
                                 shinycssloaders::withSpinner(uiOutput(outputId = "SB_UI"),
                                                              type = 5)
                        ),
                        "----",
                        "Sterol lipids",
                        tabPanel(title = "Bile acids and derivatives",
                                 shinycssloaders::withSpinner(uiOutput(outputId = "BA_UI"),
                                                              type = 5)
                        ),
                        tabPanel(title = "Secosteroids ",
                                 shinycssloaders::withSpinner(uiOutput(outputId = "SC_UI"),
                                                              type = 5)
                        ),
                        tabPanel(title = "Steroid conjugates",
                                 shinycssloaders::withSpinner(uiOutput(outputId = "STC_UI"),
                                                              type = 5)
                        ),
                        tabPanel(title = "Sterols",
                                 shinycssloaders::withSpinner(uiOutput(outputId = "ST_UI"),
                                                              type = 5)
                        ),
                        tabPanel(title = "Other sterol lipids",
                                 shinycssloaders::withSpinner(uiOutput(outputId = "OST_UI"),
                                                              type = 5)
                        )
             ), # end navbarMenu identification
             # start tabpanel issues
             tabPanel(title = "Issues",
                      fluidPage(
                        fluidRow(column = 12,
                                 tableOutput(outputId = "tbl_issues_class")),
                        fluidRow(column = 12,
                                 tableOutput(outputId = "tbl_issues"))
                      )
             ), # end tabpanel issues
             # start navbarMenu analysis
             navbarMenu(title = "Analysis",
                        # start tabpanel compare samples
                        tabPanel(title = "Compare samples",
                                 fluidPage(
                                   fluidRow(
                                     column(width = 1,
                                            radioButtons(inputId = "select_z_heatmap",
                                                         label = "Select intensity scale:",
                                                         choices = c("Raw" = "raw",
                                                                     "z-score" = "zscore",
                                                                     "Total area norm." = "totnorm"),
                                                         selected = "zscore")),
                                     column(width = 11,
                                            shinycssloaders::withSpinner(plotlyOutput(outputId = "compare_samples",
                                                                                      height = "900px"),
                                                                         type = 5)))
                                 )
                        ) # end tabpanel compare samples
             ), # end navbarmenu analysis
             # tabPanel About
             navbarMenu(title = "Help",
                        tabPanel(title = "Lipids",
                                 fluidPage(
                                   fluidRow(column = 12,
                                            h3("Lipids"),
                                            p("So far this workflow handles the following lipid classes:",
                                              HTML("<ul>
                                                               <li>Phospholipids (PA, PC, PE, PG, PI, PS)</li>
                                                               <li>Lysophospholipids (LPA, LPC, LPE, LPG, LPI, LPS)</li>
                                                               <li>Etherphospholipids (etherLPC, etherLPE, etherPC, etherPE, etherPG, etherPI)</li>
                                                               <li>Oxidezed phopholidids (OxPC, OxPE, OxPG, OxPI, OxPS)</li>
                                                               <li>Phosphatidyl(m)ethanol (PEtOH, PMeOH)</li>
                                                               <li>Acylsphingomeyelins (ASM)</li>
                                                               <li>Sphingomeyelins (SM)</li>
                                                               <li>Ceramides (Cer-AP, Cer-AS, Cer-BS, Cer-EODS, Cer-EOS, Cer-NDS, Cer-NS, Cer-NP)</li>
                                                               <li>Neutral glycosphingolipids (AHexBRS, AHexCAS, AHexCer, AHexCS, AHexSIS, Hex2Cer, Hex3Cer, HexCer-AP, HexCer-EOS, HexCer-HDS, HexCer-HS, HexCer-NS)</li>
                                                               <li>Phoshosphingolipids (PE-Cer, PI-Cer)</li>
                                                               <li>Fatty acids and Fatty acid ester of hydroxyl fatty acid</li>
                                                               <li>Glycerolipids (TG, DG, MG)</li>
                                                               <li>Glycosyldiradylglycerols (DGDG, MGDG, SQDG, EtherSMGDG)</li>
                                                               <li>Other glycerolipids (ADGGA, DGGA, DGCC, DGTS, LDGTS, LDGTA)</li>
                                                               <li>Ether glycerolipids (etherTG, etherDG)</li>
                                                               <li>triradylglyceroles, estolides (TG-EST)</li>
                                                               <li>Cardiolipins (CL, MLCL, DLCL)</li>
                                                               <li>(Hemi)bismonoacylglycerophosphate (HBMP, BMP)</li>
                                                               <li>Cholesterol esters (CE)</li>
                                                               <li>Vitamin A fatty acid ester (VAE)</li>
                                                               <li>Coenzyme Q (CoQ)</li>
                                                               <li>Sphingoid bases (Sph, DHSph, PhytoSph, SL)</li>
                                                               <li>Sterol sulfate (SSulfate)</li>
                                                               <li>N-acyl-lysophosphatidylethanolamine (LNAPE)</li>
                                                               <li>Fatty amides (NAE, NAGly, NAGlySer, NAOrn, CAR)</li>
                                                               <li>Sterols (CASE, BRSE, DEGSE, STSE, SHex)</li>
                                                               <li>Bile acid and derivatives (DCAE, GDCAE, GLCAE, KDCAE, KLCAE, TDCAE, TLCAE, BileAcid)</li>
                                                               <li>Glycerophosphoinositolglycans (Ac2PIM1, Ac2PIM2, Ac3PIM2, Ac4PIM2)</li>
                                                               <li>Acidic glycosphingolipids (GM3, SHexCer)</li>
                                                               </ul>")),
                                            p("More information about the lipid nomenclature can be found ",
                                              a("here",
                                                href="https://prime.psc.riken.jp/compms/msdial/lipidnomenclature.html",
                                                target ="_blank"),
                                              "."))
                                 )),
                        tabPanel(title = "About",
                                 fluidPage(
                                   fluidRow(column = 12,
                                            h3("Issues"),
                                            p("If you have any ideas to extend this shiny app please send me an email. If you have any issue please send me an email or go to the ",
                                              a("issue tracker.", href = "http://github.com/ricoderks/lipidomics/issues", target = "_blank"),
                                              "Cheers, Rico")),
                                   fluidRow(column = 12,
                                            h3("Session info"),
                                            verbatimTextOutput("about_session"))
                                 )
                        ) # end tabPanel About
             )
  )
)
