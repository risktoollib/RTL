#' \code{stl_decomp}
#' @description Provides a summary of returns distribution
#' @param x Wide dataframe with date column and single series (univariate).
#' @param output "chart" to see output as a graph. "data" for results as a list.
#' @param s.window Either the character string "periodic" or the span (in lags) of the loess window for seasonal extraction, which should be odd. This has no default.
#' @param s.degree Degree of locally-fitted polynomial in seasonal extraction. Should be zero or one.
#' @param ... Other parms
#' @return a chart or list object of results
#' @export stl_decomp
#' @author Philippe Cote
#' @examples
#' x <- dflong %>% dplyr::filter(series=="CL01")
#' stl_decomp(x,output="chart",s.window=13,s.degree=1)
#' stl_decomp(x,output="data",s.window=13,s.degree=1)

stl_decomp <- function(x = x,output = "chart",s.window = 13,s.degree = 1,...) {

  tmp.name<-unique(x$series)
  x.ts <- tibbletime::as_tbl_time(x, date) %>% dplyr::select(-series) %>% tibbletime::collapse_by("monthly") %>%
    dplyr::group_by(date) %>% dplyr::summarise_all(mean)

  x.ts <- stats::ts(data=x.ts$value,
    start = c(lubridate::year(dplyr::first(x$date)),lubridate::month(dplyr::first(x$date))),
    end = c(lubridate::year(dplyr::last(x$date)),lubridate::month(dplyr::last(x$date))),
    frequency = 12)

  fit <- x.ts %>% stats::stl(s.window=s.window, s.degree=s.degree,robust=TRUE)

  if (output == "chart") {
    return(fit %>% forecast::autoplot(ts.colour = 'blue') + ggplot2::ggtitle(paste("Seasonal / Trend Decomposition of",tmp.name)) + ggplot2::xlab(""))
    }

  if (output == "data") {return(fit)}

}
