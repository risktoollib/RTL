#' Z-Score applied to seasonal data divergence
#' @description
#' Supports analytics and display of seasonal data. Z-Score is
#' computed on residuals conditional on their seasonal period.
#' Beware that most seasonal charts in industry e.g. (NG Storage)
#' is not de-trended so results once you apply an STL decomposition
#' will vary from the unajusted seasonal plot.
#' @param df Long data frame with columns series, date and value
#' @param title Default is a blank space returning the unique value in df$series.
#' @param per
#' Frequency of seasonality "yearweek" (DEFAULT). "yearmonth", "yearquarter"
#' @param output
#' "stl" for STL decomposition chart,
#' "stats" for STL fitted statistics.
#' "res" for STL fitted data.
#' "zscore" for residuals Z-score,
#' "seasonal" for standard seasonal chart.
#' @param chart
#' "seasons" for feasts::gg_season() (DEFAULT)
#' "series" for feasts::gg_subseries()
#' @return Time series of STL decomposition residuals Z-Scores, or
#' standard seasonal chart with feast package.
#' @importFrom tsibble as_tsibble index_by group_by_key
#' @export chart_zscore
#' @author Philippe Cote
#' @examples
#' \dontrun{
#' df <- eiaStocks %>% dplyr::filter(series == "NGLower48")
#' title <- "NGLower48"
#' chart_zscore(df = df, title = " ", per = "yearweek", output = "stl", chart = "seasons")
#' chart_zscore(df = df, title = " ", per = "yearweek", output = "stats", chart = "seasons")
#' chart_zscore(df = df, title = " ", per = "yearweek", output = "res", chart = "seasons")
#' chart_zscore(df = df, title = " ", per = "yearweek", output = "zscore", chart = "seasons")
#' chart_zscore(df = df, title = " ", per = "yearweek", output = "seasonal", chart = "seasons")
#' }
chart_zscore <- function(df = df, title = "NG Storage Z Score", per = "yearweek", output = "zscore", chart = "seasons") {
  if (!requireNamespace("feasts", quietly = TRUE)) {stop("Package \"feasts\" needed for this function to work. Please install it.", call. = FALSE)}
  if (!requireNamespace("fabletools", quietly = TRUE)) {stop("Package \"fabletools\" needed for this function to work. Please install it.", call. = FALSE)}
  if (nchar(title) == 0) {
    title <- unique(df$series)
  }
  if (!output %in% c("zscore", "seasonal", "stats", "stl", "res")) {
    stop(print("Incorrect output parameter"))
  }
  if (!per %in% c("yearweek", "yearmonth", "yearquarter")) {
    stop(print("Incorrect period parameter"))
  }
  if (per %in% c("yearweek", "yearquarter")) {
    s <- 7
    e <- 8
  }
  if (per == "yearmonth") {
    s <- 6
    e <- 8
  }

  # s <- list(freq = ~yearweek(.))

  if (output == "stl") {
    x <- df %>%
      tsibble::as_tsibble(key = series, index = date) %>%
      tsibble::group_by_key()
    if (per %in% c("yearweek")) {
      x <- x %>% tsibble::index_by(freq = ~ yearweek(.))
    }
    if (per %in% c("yearmonth")) {
      x <- x %>% tsibble::index_by(freq = ~ yearmonth(.))
    }
    if (per %in% c("yearquarter")) {
      x <- x %>% tsibble::index_by(freq = ~ yearquarter(.))
    }
    # tsibble::index_by(freq = ~do.call(per,args=list(.))) %>%
    x <- x %>%
      dplyr::summarise(value = mean(value)) %>%
      fabletools::model(feasts::STL(value ~ season(window = Inf))) %>%
      fabletools::components() %>%
      ggplot2::autoplot() + ggplot2::ggtitle(title)
    return(x)
  }

  if (output == "stats") {
    x <- rbind(df, df %>% dplyr::mutate(series = title)) %>%
      tsibble::as_tsibble(key = series, index = date) %>%
      tsibble::group_by_key() # %>%
    if (per %in% c("yearweek")) {
      x <- x %>% tsibble::index_by(freq = ~ yearweek(.))
    }
    if (per %in% c("yearmonth")) {
      x <- x %>% tsibble::index_by(freq = ~ yearmonth(.))
    }
    if (per %in% c("yearquarter")) {
      x <- x %>% tsibble::index_by(freq = ~ yearquarter(.))
    }
    x <- x %>%
      dplyr::summarise(value = mean(value)) %>%
      fabletools::features(value, feasts::feat_stl)
    return(x)
  }

  if (output == "res") {
    x <- rbind(df, df %>% dplyr::mutate(series = title)) %>%
      tsibble::as_tsibble(key = series, index = date) %>%
      tsibble::group_by_key() # %>%
    if (per %in% c("yearweek")) {
      x <- x %>% tsibble::index_by(freq = ~ yearweek(.))
    }
    if (per %in% c("yearmonth")) {
      x <- x %>% tsibble::index_by(freq = ~ yearmonth(.))
    }
    if (per %in% c("yearquarter")) {
      x <- x %>% tsibble::index_by(freq = ~ yearquarter(.))
    }
    x <- x %>%
      dplyr::summarise(value = mean(value)) %>%
      fabletools::model(feasts::STL(value ~ season(window = Inf))) %>%
      fabletools::components()
    return(x)
  }

  df <- df %>%
    tsibble::as_tsibble(key = series, index = date) %>%
    tsibble::group_by_key() # %>%
  if (per %in% c("yearweek")) {
    df <- df %>% tsibble::index_by(freq = ~ yearweek(.))
  }
  if (per %in% c("yearmonth")) {
    df <- df %>% tsibble::index_by(freq = ~ yearmonth(.))
  }
  if (per %in% c("yearquarter")) {
    df <- df %>% tsibble::index_by(freq = ~ yearquarter(.))
  }
  df <- df %>%
    # tsibble::index_by(freq = ~do.call(per,args=list(.))) %>%
    dplyr::summarise(value = mean(value))
  z <- df %>%
    fabletools::model(feasts::STL(value ~ season(window = Inf))) %>%
    fabletools::components() %>%
    dplyr::transmute(
      per = as.numeric(do.call(stringr::str_sub, args = list(freq, start = s, end = e))),
      year = lubridate::year(freq),
      value = remainder
    ) %>%
    dplyr::as_tibble() %>%
    dplyr::group_by(per) %>%
    dplyr::summarise(u = mean(value), sigma = stats::sd(value))
  x <- df %>%
    fabletools::model(feasts::STL(value ~ season(window = Inf))) %>%
    fabletools::components() %>%
    dplyr::mutate(per = as.numeric(do.call(stringr::str_sub, args = list(freq, start = s, end = e)))) %>%
    dplyr::left_join(z, by = c("per")) %>%
    dplyr::mutate(z.score = (remainder - u) / sigma)

  if (output == "seasonal") {
    if (chart == "seasons") {
      x <- df %>% feasts::gg_season(value) + ggplot2::ggtitle(title)
    }
    if (chart == "series") {
      x <- df %>% feasts::gg_subseries(value) + ggplot2::ggtitle(title)
    }
  }

  if (output == "zscore") {
    pal <- c("red", "orange", "green", "orange", "red")

    x <- x %>%
      dplyr::mutate(date = as.Date(freq)) %>%
      #dplyr::filter(date > ) %>%
      plotly::plot_ly(x = ~date, y = ~z.score, color = ~z.score, colors = pal) %>%
      plotly::add_bars() %>%
      plotly::layout(
        title = list(text = title, x = 0),
        xaxis = list(title = "", range = c(Sys.Date() - months(120), Sys.Date())),
        yaxis = list(title = "Z Score of Seasonally-Adjusted Residuals", range = c(3, -3), separators = ".,", tickformat = ".2f")
      )
  }
  return(x)
}
