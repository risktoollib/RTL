#' Cumulative performance and drawdown summary.
#' @description Multi Asset Display of Cumulative Performance and Drawdowns
#' @param ret Wide dataframe univariate or multivariate of percentage returns. `tibble`
#' @param main Chart title. `character`
#' @param geometric Use geometric returns TRUE or FALSE. `logical`
#' @param linesize Size of lines in chart and legend. `numeric`
#' @returns Cumulative performance and drawdown charts. `ggplot`
#' @export chart_PerfSummary
#' @author Philippe Cote
#' @examplesIf requireNamespace("PerformanceAnalytics", quietly = TRUE) && requireNamespace("ggplot2", quietly = TRUE)
#' ret <- data.frame(
#'   date = seq.Date(Sys.Date() - 60, Sys.Date(), 1),
#'   CL01 = rnorm(61, 0, .01), RB01 = rnorm(61, 0, 0.02)
#' )
#' chart_PerfSummary(ret = ret,
#' geometric = TRUE,
#' main = "Cumulative Returns and Drawdowns",
#' linesize = 1.25)
chart_PerfSummary <- function(ret = ret, geometric = TRUE, main = "Cumulative Returns and Drawdowns", linesize = 1.25) {
  if (!requireNamespace("PerformanceAnalytics", quietly = TRUE)) {stop("Package \"PerformanceAnalytics\" needed for this function to work. Please install it.", call. = FALSE)}
  if (!requireNamespace("ggplot2", quietly = TRUE)) {stop("Package \"ggplot2\" needed for this function to work. Please install it.", call. = FALSE)}
  ret <- xts::xts(ret[, -1], order.by = ret[, 1])

  if (geometric == TRUE) {
    cumret <- cumprod(ret + 1)
  } else {
    cumret <- cumsum(ret)
  }
  Drawdowns <- PerformanceAnalytics::Drawdowns(ret, geometric)

  cumret <- dplyr::as_tibble(cumret) %>%
    dplyr::mutate(date = zoo::index(cumret)) %>%
    tidyr::pivot_longer(-date, names_to = "series", values_to = "value")
  cumret$variable <- "CumRet"

  drawd <- dplyr::as_tibble(Drawdowns) %>%
    dplyr::mutate(date = zoo::index(Drawdowns)) %>%
    tidyr::pivot_longer(-date, names_to = "series", values_to = "value")
  drawd$variable <- "Dranwdowns"

  df <- rbind(drawd, cumret)
  df %>% ggplot2::ggplot(ggplot2::aes(x = date, y = value, color = series)) +
    ggplot2::facet_grid(variable ~ ., scales = "free", space = "free") +
    ggplot2::geom_line() +
    ggplot2::ggtitle(main) +
    ggplot2::xlab("") +
    ggplot2::ylab("")
}
