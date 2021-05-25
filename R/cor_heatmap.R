#' @title Create correlation heatmap of all samples
#'
#' @description Create a correlation heatmap of all samples..
#'
#' @param df tibble in tidy format
#'
#' @return pheatmap object
#'
#' @importFrom magrittr %>%
#' @importFrom dplyr select matches
#' @importFrom stringr str_extract
#' @importFrom rlang .data
#' @importFrom pheatmap pheatmap
#' @importFrom stats cor
#'
#'
#' @author Rico Derks
#'
cor_heatmap <- function(df) {
  df_m <- df %>%
    select(matches("([qQ][cC]pool|[sS]ample)"))

  cormat <- cor(df_m)

  # Define which pheno data columns should be highlighted in the plot
  ann <- data.frame(sample_type = str_extract(string = colnames(df_m),
                                              pattern = "([qQ][cC]pool|[sS]ample)"))
  rownames(ann) <- colnames(df_m)

  # show heatmap
  p <- pheatmap(cormat,
           annotation = ann,
           cluster_cols = FALSE,
           cluster_rows = FALSE,
           fontsize_row = 8,
           fontsize_col = 6)

  return(p)
}
