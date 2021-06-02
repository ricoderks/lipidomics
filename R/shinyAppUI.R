#' @title Shiny app server object
#'
#' @description This is the UI function for the shiny app.
#'
#' @import shiny

# create the shiny application user interface
shinyAppUI <- navbarPage(title = "CPM - Lipidomics",
                         id = "navbar_selection",
                         # tabPanel Files
                         tabPanel(title = "Files",
                                  fluidPage(
                                    fluidRow(column = 12,
                                             h4("MS-DIAL files")),
                                    fluidRow(
                                      column(width = 12,
                                             fileInput(inputId = "res_file_pos",
                                                       label = "Positive mode:",
                                                       multiple = FALSE,
                                                       accept = c(".txt"),
                                                       width = 400),
                                             fileInput(inputId = "res_file_neg",
                                                       label = "Negative mode:",
                                                       multiple = FALSE,
                                                       accept = c(".txt"),
                                                       width = 400),
                                             style = "background-color: #E8E8E8")
                                    ),
                                    fluidRow(column = 12,
                                             h4("Lipid classes")),
                                    fluidRow(
                                      # verbatimTextOutput(outputId = "lipid_classes"),
                                      uiOutput(outputId = "select_lipid_classes"),
                                      style = "background-color: #E8E8E8"
                                    )
                                  )
                         ), # end tabPanel Files
                         # tabpanel Lipid data
                         tabPanel(title = "Lipid data",
                                  fluidPage(
                                    fluidRow(column = 12,
                                             tableOutput(outputId = "lipid_data_table"))
                                  )
                         ), # end tabPanel Data
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
                                                        uiOutput(outputId = "rsd_lipidclass_ui")
                                               )
                                             )
                                    ), # end tabpanel QC lipid class
                                    #tabpanel correlation plot
                                    tabPanel(title = "QC - Correlation",
                                             fluidPage(
                                               fluidRow(column = 12,
                                                        uiOutput(outputId = "corplot")
                                               )
                                             )
                                    ) # end tabpanel correlation plot
                         ), # end navbarMenu QC
                         # navbarMenu identification
                         navbarMenu(title = "Identification",
                                    "Fatty acyls",
                                    tabPanel(title = "Fatty acids and conjugates",
                                             uiOutput(outputId = "FA_UI")
                                    ),
                                    tabPanel(title = "Fatty amides",
                                             uiOutput(outputId = "FAM_UI")
                                    ),
                                    tabPanel(title = "Fatty esters",
                                             uiOutput(outputId = "FE_UI")
                                    ),
                                    "----",
                                    "Glyceroplipids",
                                    tabPanel(title = "Ether/Oxidized glycerolipids",
                                             uiOutput(outputId = "EGL_UI")
                                    ),
                                    tabPanel(title = "Glycerolipids",
                                             uiOutput(outputId = "GL_UI")
                                    ),
                                    tabPanel(title = "Glycosyldiradylglycerols ",
                                             uiOutput(outputId = "GLDG_UI")
                                    ),
                                    tabPanel(title = "Other glycerolipids ",
                                             uiOutput(outputId = "OGL_UI")
                                    ),
                                    "----",
                                    "Glycerophospholipids",
                                    tabPanel(title = "Glycerophosphates (PA)",
                                             uiOutput(outputId = "PA_UI")
                                    ),
                                    tabPanel(title = "Glycerophosphocholines (PC)",
                                             uiOutput(outputId = "PC_UI")
                                    ),
                                    tabPanel(title = "Glycerophosphoethanolamines (PE)",
                                             uiOutput(outputId = "PE_UI")
                                    ),
                                    tabPanel(title = "Glycerophosphoglycerols (PG)",
                                             uiOutput(outputId = "PG_UI")
                                    ),
                                    tabPanel(title = "Glycerophosphoglycerophosphoglycerols (CL)",
                                             uiOutput(outputId = "CL_UI")
                                    ),
                                    tabPanel(title = "Glycerophosphoinositolglycans",
                                             uiOutput(outputId = "AcPIM_UI")
                                    ),
                                    tabPanel(title = "Glycerophosphoinositols (PI)",
                                             uiOutput(outputId = "PI_UI")
                                    ),
                                    tabPanel(title = "Glycerophosphoserines (PS)",
                                             uiOutput(outputId = "PS_UI")
                                    ),
                                    tabPanel(title = "Other glycerophospholipids",
                                             uiOutput(outputId = "OGPL_UI")
                                    ),
                                    tabPanel(title = "Oxidized glycerophospholipids",
                                             uiOutput(outputId = "OPL_UI")
                                    ),
                                    "----",
                                    tabPanel(title = "Prenol lipids",
                                             uiOutput(outputId = "PRL_UI")
                                    ),
                                    "----",
                                    "Sphingolipids",
                                    tabPanel(title = "Acidic glycosphingolipids",
                                             uiOutput(outputId = "AcGL_UI")
                                    ),
                                    tabPanel(title = "Ceramides ",
                                             uiOutput(outputId = "Cer_UI")
                                    ),
                                    tabPanel(title = "Phosphosphingolipids ",
                                             uiOutput(outputId = "PSL_UI")
                                    ),
                                    tabPanel(title = "Neutral glycosphingolipids",
                                             uiOutput(outputId = "NPSL_UI")
                                    ),
                                    tabPanel(title = "Sphingoid bases",
                                             uiOutput(outputId = "SB_UI")
                                    ),
                                    "----",
                                    "Sterol lipids",
                                    tabPanel(title = "Bile acids and derivatives",
                                             uiOutput(outputId = "BA_UI")
                                    ),
                                    tabPanel(title = "Secosteroids ",
                                             uiOutput(outputId = "SC_UI")
                                    ),
                                    tabPanel(title = "Steroid conjugates",
                                             uiOutput(outputId = "STC_UI")
                                    ),
                                    tabPanel(title = "Sterols",
                                             uiOutput(outputId = "ST_UI")
                                    ),
                                    tabPanel(title = "Other sterol lipids",
                                             uiOutput(outputId = "OST_UI")
                                    )
                         ), # end navbarMenu identification
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
