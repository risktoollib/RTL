#' Computes betas of futures contracts with respect to the 1st line contract
#' @description Returns betas of futures contracts versus front futures contract.
#' @param x Wide dataframe with date column and multiple series columns (multivariate).
#' @param period "all" or numeric period of time in last n periods as character eg "100".
#' @param betatype "all" "bull" "bear".
#' @param output "betas" or "chart"
#' @return betas data frame or plotly chart of betas
#' @importFrom readr parse_number
#' @export promptBeta
#' @author Philippe Cote
#' @examples
#' \dontrun{
#'  x <- dflong %>%
#'  dplyr::filter(grepl("CL",series)) %>%
#'  dplyr::mutate(series = readr::parse_number(series)) %>% dplyr::group_by(series) %>%
#'  RTL::returns(df = ., retType = "abs",period.return = 1,spread = TRUE) %>%
#'  RTL::rolladjust(x = .,commodityname = c("cmewti"),rolltype = c("Last.Trade")) %>%
#'  # removing the day it prices went negative...
#'  dplyr::filter(!date %in% c(as.Date("2020-04-20"),as.Date("2020-04-21")))
#' promptBeta(x = x, period = "all", betatype = "all", output = "chart")
#' promptBeta(x = x, period = "all", betatype = "bull", output = "betas")
#' promptBeta(x = x, period = "100", betatype = "bear", output = "betas")
#' }
promptBeta <- function(x = x, period = "all", betatype = "all", output = "chart") {

  # if (!requireNamespace("PerformanceAnalytics", quietly = TRUE)) {
  #   stop("Package \"PerformanceAnalytics\" needed for this function to work. Please install it.",
  #        call. = FALSE)
  # }

  term <- stats::na.omit(as.numeric(gsub("[^0-9]", "", colnames(x))))

  if (period != "all") {
    period <- as.numeric(period)
    x <- x %>%
      dplyr::slice_max(n = period, order_by = date) %>%
      dplyr::arrange(date)
  }
  x <- as.data.frame(x)
  ret <- xts::xts(x[, -1], order.by = x[, 1])
  all <- PerformanceAnalytics::CAPM.beta(ret, ret[, 1])
  bull <- PerformanceAnalytics::CAPM.beta.bull(ret, ret[, 1])
  bear <- PerformanceAnalytics::CAPM.beta.bear(ret, ret[, 1])

  n <- 1:nrow(t(all))
  f <- data.frame(Beta = t(all), Prompt = n)
  names(f) <- c("Beta", "Prompt")

  # betaformula <- "Only applicable when computing term betas and estimating an exponential fit along the term"
  # betaformulaObject <- "Only applicable when computing term betas and estimating an exponential fit along the term"
  # betaformula <- summary(stats::nls(Beta ~ a + exp(b*Prompt),data = f,start = list(a = 0,b = 0)))
  # betaformulaObject <- stats::nls(Beta ~ a + exp(b*Prompt),data = f,start = list(a = 0,b = 0))

  out <- cbind(t(all), t(bull), t(bear))
  out <- data.frame(out)
  names(out) <- c("all", "bull", "bear")

  df <- out
  rownames(df) <- NULL
  df$contract <- term

  chart <- df %>%
    tidyr::pivot_longer(-contract, names_to = "series", values_to = "value") %>%
    plotly::plot_ly(x = ~contract, y = ~value, name = ~series, color = ~series) %>%
    plotly::add_lines() %>%
    plotly::layout(
      title = list(text = "Contract Betas vs Front Contract", x = 0),
      xaxis = list(title = ""),
      yaxis = list(title = "")
    )

  if (output == "betas") {
    return(df)
  }
  if (output == "chart") {
    return(chart)
  }
}
