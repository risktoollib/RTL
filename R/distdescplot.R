#' \code{distdescplot}
#' @description Provides a summary of returns distribution
#' @param x Wide dataframe with date column and single series (univariate).
#' @return Multiple plots describing the distribution.
#' @export distdescplot
#' @author Philippe Cote
#' @examples
#' x <- dflong %>% dplyr::filter(series=="CL01")
#' x <- returns(df=x,retType="rel",period.return=1,spread=TRUE)
#' x <- rolladjust(x=x,commodityname=c("cmewti"),rolltype=c("Last.Trade"))
#' distdescplot(x=x)

distdescplot <- function(x=x){
  x <- xts::as.xts(x[, 2], order.by = x$date)
  x.stationary <- x-mean(x)
  fit <- fGarch::garchFit(~garch(1, 1), data = x.stationary, trace = FALSE)
  if (xts::periodicity(x)$scale=="daily") {garchvol <- fit@sigma.t * sqrt(252)}
  if (xts::periodicity(x)$scale=="weekly") {garchvol <- fit@sigma.t * sqrt(52)}
  if (xts::periodicity(x)$scale=="monthly") {garchvol <- fit@sigma.t * sqrt(12)}
  voldata <- merge(x, garchvol)
  colnames(voldata) <- c("returns", "garch vol")
  voldata$CumulativeReturn <- cumsum(voldata$returns)
  voldata$Drawdowns <- PerformanceAnalytics::Drawdowns(voldata$returns)
  def.par <- par(no.readonly = TRUE)
  on.exit(par(def.par))
  graphics::layout(matrix(c(1,2,3,2), 2, 2, byrow = TRUE))
  PerformanceAnalytics::chart.Histogram(as.numeric(zoo::coredata(x)), main = "Return Distribution",xlim = c(-max(abs(x)), max(abs(x))),
                                        methods = c("add.rug","add.normal", "add.centered", "add.density", "add.risk"))
  print(xts::plot.xts(voldata, multi.panel=4, type = "h", col=c("blueviolet", "blue", "deepskyblue2","cornflowerblue"), lty=1:4, yaxis.same = FALSE))
  fitdistrplus::descdist(as.numeric(zoo::coredata(x)), boot = nrow(x), discrete = FALSE)
}
