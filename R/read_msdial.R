#' @title Read MS-DIAl files
#'
#' @description Read the result files from MS-DIAL.
#'
#' @param filename The filename of the MS-DIAL file.
#'
#' @return Returns a tibble
#'
#' @importFrom readr read_delim cols col_double col_character col_integer
#'
#' @author Rico Derks
#'
read_msdial <- function(filename) {
  # determine which version is loaded, read only the column names
  column_names <- colnames(readr::read_delim(file = filename,
                                    delim ="\t",
                                    na = c("", "NA", "null"),
                                    n_max = 1,
                                    skip = 4))

  if("Simple dot product" %in% column_names) {
    # version > 5.1
    res <- readr::read_delim(file = filename,
                             delim ="\t",
                             na = c("", "NA", "null"),
                             col_types = readr::cols(`Alignment ID` = readr::col_integer(),
                                                     `Average Rt(min)` = readr::col_double(),
                                                     `Average Mz` = readr::col_double(),
                                                     `Metabolite name` = readr::col_character(),
                                                     `Adduct type` = readr::col_character(),
                                                     `Post curation result` = readr::col_character(),
                                                     `Fill %` = readr::col_double(),
                                                     `MS/MS assigned` = readr::col_character(),
                                                     `Reference RT` = readr::col_character(),
                                                     `Reference m/z` = readr::col_character(),
                                                     Formula = readr::col_character(),
                                                     Ontology = readr::col_character(),
                                                     INCHIKEY = readr::col_character(),
                                                     SMILES = readr::col_character(),
                                                     `Annotation tag (VS1.0)` = readr::col_character(),
                                                     `RT matched` = readr::col_character(),
                                                     `m/z matched` = readr::col_character(),
                                                     `MS/MS matched` = readr::col_character(),
                                                     Comment = readr::col_character(),
                                                     `Manually modified for quantification` = readr::col_character(),
                                                     `Manually modified for annotation` = readr::col_character(),
                                                     `Isotope tracking parent ID` = readr::col_character(),
                                                     `Isotope tracking weight number` = readr::col_character(),
                                                     `Total score` = readr::col_double(),
                                                     `RT similarity` = readr::col_double(),
                                                     `Simple dot product` = readr::col_double(),
                                                     `Reverse dot product` = readr::col_double(),
                                                     `Weighted dot product` = readr::col_double(),
                                                     `Matched peaks percentage` = readr::col_double(),
                                                     `S/N average` = readr::col_double(),
                                                     `Spectrum reference file name` = readr::col_character(),
                                                     `MS1 isotopic spectrum` = readr::col_character(),
                                                     `MS/MS spectrum` = readr::col_character()),
                             skip = 4)
  } else {
    # version 4.x
    res <- readr::read_delim(file = filename,
                             delim ="\t",
                             na = c("", "NA", "null"),
                             col_types = readr::cols(`Alignment ID` = readr::col_integer(),
                                                     `Average Rt(min)` = readr::col_double(),
                                                     `Average Mz` = readr::col_double(),
                                                     `Metabolite name` = readr::col_character(),
                                                     `Adduct type` = readr::col_character(),
                                                     `Post curation result` = readr::col_character(),
                                                     `Fill %` = readr::col_double(),
                                                     `MS/MS assigned` = readr::col_character(),
                                                     `Reference RT` = readr::col_character(),
                                                     `Reference m/z` = readr::col_character(),
                                                     Formula = readr::col_character(),
                                                     Ontology = readr::col_character(),
                                                     INCHIKEY = readr::col_character(),
                                                     SMILES = readr::col_character(),
                                                     `Annotation tag (VS1.0)` = readr::col_character(),
                                                     `RT matched` = readr::col_character(),
                                                     `m/z matched` = readr::col_character(),
                                                     `MS/MS matched` = readr::col_character(),
                                                     Comment = readr::col_character(),
                                                     `Manually modified for quantification` = readr::col_character(),
                                                     `Manually modified for annotation` = readr::col_character(),
                                                     `Isotope tracking parent ID` = readr::col_character(),
                                                     `Isotope tracking weight number` = readr::col_character(),
                                                     `Total score` = readr::col_double(),
                                                     `RT similarity` = readr::col_double(),
                                                     `Dot product` = readr::col_double(),
                                                     `Reverse dot product` = readr::col_double(),
                                                     `Fragment presence %` = readr::col_double(),
                                                     `S/N average` = readr::col_double(),
                                                     `Spectrum reference file name` = readr::col_character(),
                                                     `MS1 isotopic spectrum` = readr::col_character(),
                                                     `MS/MS spectrum` = readr::col_character()),
                             skip = 4)
  }



  return(res)
}
