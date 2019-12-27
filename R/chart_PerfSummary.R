#' \code{chart_PerformanceSummary}
#' @description Multi Asset Display of Cumulative Performance and Drawdowns
#' @param ret Wide dataframe univariate or multivariate of percentage returns.
#' @param main Chart title.
#' @param geometric Use geometric returns TRUE or FALSE.
#' @param linesize Size of lines in chart and legend.
#' @return Cumulative performance and drawdown charts.
#' @export chart_PerfSummary
#' @author Philippe Cote
#' @examples
#' df <- dflong %>% dplyr::filter(series %in% c("CL01","CL12","CL36"))
#' ret <- returns(df=df,retType="rel",period.return=1,spread=TRUE)
#' ret <-data.frame(rolladjust(x=ret,commodityname=c("cmewti"),rolltype=c("Last.Trade")))
#' chart_PerfSummary(ret=ret, geometric=TRUE, main="Cumulative Returns and Drawdowns",linesize=1.25)

chart_PerfSummary <- function(ret=ret, geometric=TRUE, main="Cumulative Returns and Drawdowns",linesize=1.25){

  ret <- xts::xts(ret[,-1],order.by = ret[,1])

  if (geometric==TRUE) {cumret<-cumprod(ret+1)} else {cumret<-cumsum(ret)}
  Drawdowns<-PerformanceAnalytics::Drawdowns(ret,geometric)

  cumret <-dplyr::as_tibble(cumret) %>% dplyr::mutate(date=zoo::index(cumret)) %>% tidyr::gather(series,value,-date)
  cumret$variable = "CumRet"

  drawd <-dplyr::as_tibble(Drawdowns) %>% dplyr::mutate(date=zoo::index(Drawdowns)) %>% tidyr::gather(series,value,-date)
  drawd$variable = "Dranwdowns"

  df<-rbind(drawd,cumret)
  df %>% ggplot2::ggplot(ggplot2::aes(x=date,y=value,color=series)) +
    ggplot2::facet_grid(variable ~ ., scales="free", space="free") + ggplot2::geom_line() +
    ggplot2::ggtitle(main) + ggplot2::xlab("") + ggplot2::ylab("")
}
