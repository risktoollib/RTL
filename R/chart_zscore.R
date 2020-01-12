#' \code{chart_zscore}
#' @description Provides a summary of returns distribution
#' @param df Long data frame with columns series, date and value
#' @param title Your chart title
#' @param per Frequency of seasonality "weekly" (DEFAULT) or "monthly"
#' @param output "zscore" or "seasonal" chart.
#' @return Time series of STL decomposition residuals Z-Scores, or
#' standard seasonal chart with feast package.
#' @export chart_zscore
#' @author Philippe Cote
#' @examples
#' chart_zscore(df = ng_storage, title = "NG Storage Z Score", per = "weekly", output = "zscore")
#' chart_zscore(df = ng_storage, title = "NG Storage Z Score", per = "weekly", output = "seasonal")
#' chart_zscore(df = ng_storage, title = "NG Storage Z Score", per = "montlhy", output = "zscore")

chart_zscore <- function(df = df, title = "NG Storage Z Score", per = "weekly", output = "zscore") {

  if (per == "weekly") {
    df <- df %>%
      tsibble::as_tsibble(key=series, index = date) %>%
      tsibble::group_by_key() %>%
      tsibble::index_by(freq = ~yearweek(.)) %>%
      dplyr::summarise(value = mean(value))
    z <- df %>%
      feasts::STL(value ~ season(window = Inf)) %>%
      dplyr::transmute(per = as.numeric(stringr::str_sub(freq,start=7,end=8)),
                       year = lubridate::year(freq),
                       value=remainder) %>%
      dplyr::as_tibble() %>%
      dplyr::group_by(per) %>%
      dplyr::summarise(u = mean(value), sigma = stats::sd(value))
    x <- df %>%
      feasts::STL(value ~ season(window = Inf)) %>%
      dplyr::mutate(per = as.numeric(stringr::str_sub(freq,start=7,end=8))) %>%
      dplyr::left_join(z, by = c("per")) %>%
      dplyr::mutate(z.score = (remainder - u) / sigma)
  } else {
    df <- df %>%
      tsibble::as_tsibble(key=series, index = date) %>%
      tsibble::group_by_key() %>%
      tsibble::index_by(freq = ~yearmonth(.)) %>%
      dplyr::summarise(value = mean(value))
    z <- df %>%
      feasts::STL(value ~ season(window = Inf)) %>%
      dplyr::transmute(per = stringr::str_sub(freq,start=6,end=8),
                       year = lubridate::year(freq),
                       value=remainder) %>%
      dplyr::as_tibble() %>%
      dplyr::group_by(per) %>%
      dplyr::summarise(u = mean(value), sigma = stats::sd(value))
    x <- df %>%
      feasts::STL(value ~ season(window = Inf)) %>%
      dplyr::mutate(per = stringr::str_sub(freq,start=6,end=8)) %>%
      dplyr::left_join(z, by = c("per")) %>%
      dplyr::mutate(z.score = (remainder - u) / sigma)
  }

   #%>%
    #dplyr::mutate(change = value - dplyr::lag(value))

  if (output == "seasonal") {
    x <- df %>% feasts::gg_subseries(value)
  }

  if (output == "zscore") {

    pal <- c("red","orange", "green", "orange","red")
    max.d = as.Date(max(df$freq))
    min.d = as.Date(min(df$freq))

    x <-  x %>%
      dplyr::filter(freq >= min(freq)) %>%
      plotly::plot_ly(x = ~freq, y = ~z.score, color = ~z.score, colors = pal) %>%
      plotly::add_markers() %>%
      plotly::layout(title = list(text= title, x = 0),
                     xaxis = list(title = "", range = c(min.d,max.d)),
                     yaxis = list(title = "Z Score of Seasonally-Adjusted Residuals", range = c(3,-3) ,separators = '.,',tickformat = ".2f"))
  }
  return(x)
}
