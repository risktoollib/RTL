#' \code{promptBeta}
#' @description Returns betas of multiple xts prices (by using relative returns).
#' @param x Wide dataframe with date column and multiple series columns (multivariate).
#' @param period "all" or numeric period of time in last n periods.
#' @param betatype "all" "bull" "bear".
#' @param output "betas", "chart","stats"
#' @return ggplot chart, df of betas or stats
#' @export promptBeta
#' @author Philippe Cote
#' @examples
#' \dontrun{
#' x <- dflong %>% dplyr::filter(grepl("CL",series))
#' x <- x %>% dplyr::mutate(series=readr::parse_number(series)) %>% dplyr::group_by(series)
#' x <- returns(df=x,retType="abs",period.return=1,spread=TRUE)
#' x <- rolladjust(x=x,commodityname=c("cmewti"),rolltype=c("Last.Trade"))
#' promptBeta(x=x,period="all",betatype="all",output="chart")
#' promptBeta(x=x,period="all",betatype="all",output="betas")
#' promptBeta(x=x,period="all",betatype="all",output="stats")
#' }


promptBeta<-function(x=x,period="all",betatype="all",output="chart") {

  term = stats::na.omit(as.numeric(gsub("[^0-9]","",colnames(x))))

  if (is.numeric(period)) {x <- x %>% dplyr::filter(dplyr::last(period))}
  x <- as.data.frame(x)
  ret <- xts::xts(x[,-1],order.by = x[,1])
  all <- PerformanceAnalytics::CAPM.beta(ret,ret[,1])
  bull <- PerformanceAnalytics::CAPM.beta.bull(ret,ret[,1])
  bear <- PerformanceAnalytics::CAPM.beta.bear(ret,ret[,1])

  n<-1:nrow(t(all))
  f<-data.frame(Beta=t(all),Prompt=n);names(f)<-c("Beta","Prompt")

  betaformula <- "Only applicable when computing term betas and estimating an exponential fit along the term"
  betaformulaObject <- "Only applicable when computing term betas and estimating an exponential fit along the term"

  betaformula<-summary(stats::nls(Beta ~ exp(a+b*Prompt),data=f,start=list(a=0,b=0)))
  betaformulaObject<-stats::nls(Beta ~ exp(a+b*Prompt),data=f,start=list(a=0,b=0))

  out<-cbind(t(all),t(bull),t(bear))
  out<-data.frame(out);names(out)<-c("all","bull","bear")

  df <- out
  rownames(df) <- NULL
  df$contract <- term

  chart <- df %>%
    tidyr::pivot_longer(-contract, names_to = "series",values_to = "value") %>%
    plotly::plot_ly(x = ~contract, y = ~value, name = ~series, color = ~series) %>%
    plotly::add_lines() %>%
    plotly::layout(title = list(text = "Contract Betas vs Front Contract", x = 0),
                   xaxis = list(title = ""),
                   yaxis = list(title = ""))
    # ggplot2::ggplot(ggplot2::aes(x=contract,y=value,col=series)) + ggplot2::geom_line() +
    # ggplot2::theme(legend.position="top") + ggplot2::ylim(0,1.1) +
    # ggplot2::labs(title="Contract Betas vs Front Contract",
    #      subtitle="Bear (Bull) = Beta in Down (Up) Moves ",
    #      caption="",
    #      y="Beta", x="Contract")

  if (output=="betas") {return(df)}
  if (output=="chart") {return(chart)}
  if (output=="stats") {
    stats <- list(betaformula = betaformula,betaformulaObject = betaformulaObject)
    return(stats)
    }
}
