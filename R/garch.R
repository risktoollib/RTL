#' \code{garch}
#' @description Computes annualised Garch(1,1) volatilities using fGarch package.
#' @param x Wide dataframe with date column and single series (univariate).
#' @param out "chart" to return chart, "data" to return data or "fit" for garch fit output
#' @return plot.xts object or xts series
#' @export garch
#' @author Philippe Cote
#' @examples
#' \dontrun{
#' x <- dflong %>% dplyr::filter(series=="CL01")
#' x <- returns(df=x,retType="rel",period.return=1,spread=TRUE)
#' x <- rolladjust(x=x,commodityname=c("cmewti"),rolltype=c("Last.Trade"))
#' summary(garch(x=x,out="fit"))
#' garch(x=x,out="chart")
#' garch(x=x,out="data")
#' }


garch <- function(x = x, out = TRUE) {

  if (!requireNamespace("rugarch", quietly = TRUE)) {
    stop("Package \"rugarch\" needed for this function to work. Please install it.",
         call. = FALSE)
  }

  x <- xts::as.xts(x[, 2], order.by = x$date)
  x <- x - mean(x)
  # fit <- fGarch::garchFit(~garch(1, 1), data = x, trace = FALSE)
  # if (xts::periodicity(x)$scale=="daily") {garchvol <- fit@sigma.t * sqrt(252)}
  # if (xts::periodicity(x)$scale=="weekly") {garchvol <- fit@sigma.t * sqrt(52)}
  # if (xts::periodicity(x)$scale=="monthly") {garchvol <- fit@sigma.t * sqrt(12)}
  fit <-  rugarch::ugarchfit(data = x, spec = rugarch::ugarchspec(), solver = "hybrid")
  if (xts::periodicity(x)$scale == "daily") {garchvol <- fit@fit$sigma * sqrt(252)}
  if (xts::periodicity(x)$scale == "weekly") {garchvol <- fit@fit$sigma * sqrt(52)}
  if (xts::periodicity(x)$scale == "monthly") {garchvol <- fit@fit$sigma * sqrt(12)}

  voldata <- merge(x, garchvol)
  colnames(voldata) <- c("returns", "garch")
  if (out == "data") {return(voldata)} else if (out == "fit") {return(fit)} else {
    xts::plot.xts(voldata[, 2], main = paste("Period Returns and Annualized Garch(1,1) for",colnames(x)[1]),ylim = c(-max(abs(voldata[, 1])), max(abs(voldata))))
    xts::addSeries(voldata[, 1], type = "h", col = "blue")
  }
}


