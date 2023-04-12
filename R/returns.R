#' Compute absolute, relative or log returns.
#' @description Computes periodic returns from a dataframe ordered by date
#' @param df Long dataframe with colnames = c("date","value","series"). `character`
#' @param retType "abs" for absolute, "rel" for relative, or "log" for log returns. `character`
#' @param period.return Number of rows over which to compute returns. `numeric`
#' @param spread TRUE if you want to spread into a long dataframe. `logical`
#' @return A dataframe object of returns. `tibble`
#' @export returns
#' @author Philippe Cote
#' @examples
#' x <- dflong %>% dplyr::filter(grepl("CL01", series))
#' returns(df = x, retType = "abs", period.return = 1, spread = TRUE)
returns <- function(df = dflong, retType = "abs", period.return = 1, spread = FALSE) {
  if (length(setdiff(colnames(df), c("date", "value", "series"))) > 0) stop("df is either not long or colnames incorrect")

  if (retType == "abs") {
    df <- df %>%
      dplyr::arrange(date) %>%
      dplyr::group_by(series) %>%
      dplyr::mutate(returns = value - dplyr::lag(value, period.return)) %>%
      stats::na.omit()
  }
  if (retType == "rel") {
    df <- df %>%
      dplyr::arrange(date) %>%
      dplyr::group_by(series) %>%
      dplyr::mutate(returns = (value / dplyr::lag(value, period.return)) - 1) %>%
      stats::na.omit()
  }
  if (retType == "log") {
    df <- df %>%
      dplyr::arrange(date) %>%
      dplyr::group_by(series) %>%
      dplyr::mutate(returns = log(value / dplyr::lag(value, period.return))) %>%
      stats::na.omit()
  }
  # df <- df %>% dplyr::filter(!date %in% sort(unique(df$date))[1])
  if (spread == TRUE) {
    df <- df %>%
      dplyr::select(date, series, returns) %>%
      tidyr::spread(series, -date)
  }
  return(df)
}
