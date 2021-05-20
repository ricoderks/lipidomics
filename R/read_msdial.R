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
  res <- read_delim(file = filename,
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
             skip = 4)

  return(res)
}
