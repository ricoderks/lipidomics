#' @title Create volcano plot
#'
#' @description Create volcano plot.
#'
#' @param lipid_data tibble with all the lipid data and test data
#' @param pvalue_adjust show the corrected p value, default is FALSE
#' @param title title of the plot
#'
#' @return plotly object
#'
#' @importFrom magrittr %>%
#' @importFrom dplyr mutate case_when
#' @importFrom rlang .data
#' @importFrom plotly plot_ly add_markers layout event_register
#' @importFrom grDevices rainbow
#'
#' @export
#'
#' @author Rico Derks
#'
volcano_plot <- function(lipid_data, pvalue_adjust = FALSE, title = "") {
  # create y-axis title
  y_title <- ifelse(pvalue_adjust == FALSE,
                    "-log10(p value)",
                    "-log10(cor. p value)")

  # create the plot
  p <- lipid_data %>%
    mutate(show_p = case_when(
      pvalue_adjust == FALSE ~ .data$p_log10,
      pvalue_adjust == TRUE ~ .data$p_log10_adj
    )) %>%
    plot_ly(x = ~fc_log2,
            y = ~show_p,
            text = ~ShortLipidName,
            colors = rainbow(n = 100),
            customdata = lipid_data$ShortLipidName,
            source = "volcano_plot_click") %>%
    add_markers(color = ~LipidClass,
                size = 3) %>%
    layout(xaxis = list(zeroline = FALSE,
                        title = "log2(fold change)"),
           yaxis = list(title = y_title),
           shapes = list(vline(-1),
                         vline(1),
                         hline(-log10(0.05))),
           legend = list(orientation = "h"),
           title = list(text = title,
                        x = 0)) %>%
    event_register(event = "plotly_click")

  return(p)
}

vline <- function(x = 0, color = "blue") {
  list(
    type = "line",
    y0 = 0,
    y1 = 1,
    yref = "paper",
    x0 = x,
    x1 = x,
    line = list(color = color,
                width = 1,
                dash = "dash")
  )
}

hline <- function(y = 0, color = "blue") {
  list(
    type = "line",
    x0 = 0,
    x1 = 1,
    xref = "paper",
    y0 = y,
    y1 = y,
    line = list(color = color,
                width = 1,
                dash = "dash")
  )
}
