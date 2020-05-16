#' \code{chart_pairs}
#' @description Pairwise scatter chart for timeseries.
#' @param df Wide data frame
#' @param title Chart title
#' @return A plotly object
#' @export chart_pairs
#' @author Philippe Cote
#' @examples
#' df <- dfwide %>% dplyr::select(date,CL01,NG01,HO01,RB01)

chart_pairs <- function(df = df, title = "Time Series Pairs Plot") {
  a <- purrr::map2(df, names(df), ~list(values=.x, label=.y))
  plotly::plot_ly(type = "splom",
                  dimensions = stats::setNames(a, NULL),
                  marker = list(color = as.integer(df$date),
                                colorscale = "Portland"),
                  showupperhalf = FALSE,
                  diagonal = list(visible = FALSE)) %>%
    plotly::layout(title = list(text = title, x = 0))
}
