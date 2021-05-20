#' @title Shiny app server object
#'
#' @description This is the UI function for the shiny app.
#'
#' @import shiny

# create the shiny application user interface
shinyAppUI <- navbarPage(title = "CPM - Lipidomics",
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
                                      column(
                                        width = 12,
                                        uiOutput(outputId = "select_lipid_classes"),
                                        style = "background-color: #E8E8E8")
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
                         # tabpanel QC
                         tabPanel(title = "QC",
                                  fluidPage(
                                    fluidRow(column = 12)
                                  )
                         ), # end tabpanel QC
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
