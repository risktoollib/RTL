#' Pairwise scatter plots for timeseries
#' @description Plots pairwise scatter plots with the time dimension.
#' Useful when exploring structural changes in timeseries properties for modeling.
#' @param df Wide data frame. `tibble`
#' @param title Chart title. `character`
#' @returns A plotly object. `htmlwidget`
#' @importFrom plotly plot_ly layout
#' @importFrom purrr map2
#' @export chart_pairs
#' @author Philippe Cote
#' @examples
#' df <- dfwide %>%
#'   dplyr::select(date, CL01, NG01, HO01, RB01) %>%
#'   tidyr::drop_na()
#' chart_pairs(df = df, title = "example")
chart_pairs <- function(df = df, title = "Time Series Pairs Plot") {
  a <- purrr::map2(df, names(df), ~ list(values = .x, label = .y))
  plotly::plot_ly(
    type = "splom",
    dimensions = stats::setNames(a, NULL),
    marker = list(
      color = as.integer(df$date),
      colorscale = "Portland"
    ),
    showupperhalf = FALSE,
    diagonal = list(visible = FALSE)
  ) %>%
    plotly::layout(title = list(text = title, x = 0))
}
