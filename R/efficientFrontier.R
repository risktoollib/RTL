#' Markowitz Efficient Frontier
#' @description Generates random portfolio weights statistics based on absolute returns.
#' @details
#' ## Commodities
#' Unlike traditional portfolio management, in commodities many transactions are
#' with derivatives (futures and swaps) and have zero or low initial investments.
#' ## Return types
#' This function is used for commodities where returns are dollars per units
#' for real assets e.g. storage tanks, pipelines...Here we measure directly
#' the periodic return in dollars per contract unit.
#' ## Empirical Finance
#' I would encourage you to pick a commodity futures contract of your choice and draw
#' a scatter plot of price level versus the daily dollar per unit change as measure
#' of risk. As a trading analyst or risk manager, then ask yourself about the
#' implications of using log returns that you then re-apply to current forward
#' curve level to arrive at a dollar risk measure per units instead of measuring
#' directly risk in dollars per unit.
#' @param nsims Number of portfolio simulations. Defaults to 5000 `numeric`
#' @param x List as provided by output of RTL::simMultivariates(). `list`
#' @param expectedReturns Defaults to NULL using periodic returns means. `numeric`
#' @returns List of portfolios and chart of efficient frontier `list`
#' @export efficientFrontier
#' @author Philippe Cote
#' @examples
#' \dontrun{
#' x =  RTL::fizdiffs %>% dplyr::select(date, dplyr::contains("WCS"))
#' efficientFrontier(nsims = 10, x = x, expectedReturns = NULL)
#' efficientFrontier(nsims = 10, x = x, expectedReturns = c(0.5,0.8,0.9))
#' }

efficientFrontier <- function(nsims = 5000, x =  RTL::fizdiffs %>% dplyr::select(date, dplyr::contains("WCS")), expectedReturns = NULL) {

  prices <- Risk <- Return <- SharpeRatio <- desc <-  NULL
  prices <- x
  ret <- x %>%
    tidyr::pivot_longer(-date, names_to = "series", values_to = "value") %>%
    dplyr::group_by(series) %>%
    dplyr::mutate(ret = value - dplyr::lag(value)) %>%
    tidyr::drop_na() %>%
    dplyr::select(-value) %>%
    tidyr::pivot_wider(names_from = series, values_from = ret)

  aves <- ret %>% dplyr::summarise_if(is.numeric, mean)

  if (!is.null(expectedReturns)) {
    for (i in 1:length(expectedReturns)) { aves[1,i] <- expectedReturns[i]}
    }

  sds <- ret %>% dplyr::summarise_if(is.numeric, stats::sd)
  corMat <- stats::cor(ret[,-1], method = "kendall")
  coVaR = diag(sds) %*% corMat %*% diag(sds)

  # means, ret, covar
  #aves <- x$ave
  #coVaR <- as.matrix(x$coVaR[,-1])

  # weights
  wts <- stats::runif(n = length(aves))
  wts <- wts/sum(wts)
  port_returns <- sum(wts * aves)
  port_risk <- sqrt(t(wts) %*% (coVaR %*% wts))
  sharpe_ratio <- port_returns/port_risk

  # port sims
  ## instantiate matrices
  singleAssets <- diag(nrow = length(aves),ncol = length(aves))
  all_wts <- matrix(nrow = nsims, ncol = length(aves))
  all_wts <- rbind(singleAssets, all_wts)
  port_returns <- vector('numeric', length = nsims + length(aves))
  port_risk <- vector('numeric', length = nsims + length(aves))
  sharpe_ratio <- vector('numeric', length = nsims + length(aves))

  for (i in seq_along(port_returns)) {
    # add port allocation for sinfle assets to show in final graph
    if (i <= length(aves)) {wts <- all_wts[i,]} else {wts <- stats::runif(length(aves))}
    # sum of weigths = 1
    wts <- wts/sum(wts)
    # Storing weight in the matrix
    all_wts[i,] <- wts
    # Portfolio returns
    port_ret <- sum(wts * aves)
    # Storing Portfolio Returns values
    port_returns[i] <- port_ret
    # Creating and storing portfolio risk
    port_sd <- sqrt(t(wts) %*% (coVaR  %*% wts))
    port_risk[i] <- port_sd
    # Creating and storing Portfolio Sharpe Ratios
    # Assuming 0% Risk free rate
    sr <- port_ret/port_sd
    sharpe_ratio[i] <- sr
  }

  port <- dplyr::tibble(Return = port_returns,
                        Risk = port_risk,
                        SharpeRatio = sharpe_ratio)


  # Converting matrix to a tibble and changing column names
  all_wts <- timetk::tk_tbl(round(all_wts,3),silent = TRUE)
  port <- timetk::tk_tbl(cbind(all_wts, port), silent = TRUE)
  names(port)[1:length(aves)] <- names(aves)

  min_var <- port[which.min(port$Risk),]
  max_sr <- port[which.max(port$SharpeRatio),]

  port <- port %>%
    tidyr::nest(data = names(port)[1:length(aves)]) %>%
    dplyr::mutate(desc = paste0(.data[["data"]]),
                  desc = stringr::str_replace_all(desc, c("list\\(" = "", "\\)" = "")))

  # p <- port %>%
  #   ggplot2::ggplot(ggplot2::aes(x = Risk, y = Return, color = SharpeRatio)) +
  #   ggplot2::geom_point() +
  #   ggplot2::theme_classic() +
  #   ggplot2::labs(x = 'Risk',
  #        y = 'Returns',
  #        title = "Portfolio Optimization & Efficient Frontier") +
  #   ggplot2::geom_point(ggplot2::aes(x = Risk,
  #                  y = Return), data = min_var, color = 'red') +
  #   ggplot2::geom_point(ggplot2::aes(x = Risk,
  #                  y = Return), data = max_sr, color = 'red') +
  #   ggplot2::annotate('text', x = min_var$Risk, y = min_var$Return, label = "")
  # p <- plotly::ggplotly(p)

  p <- port %>%
    plotly::plot_ly(
      x = ~ Risk,
      y = ~ Return,
      color = ~ SharpeRatio,
      text = ~ desc,
      hoverinfo = 'text',
      showlegend = F,
      type = "scatter",
      mode = "markers"
      )

  for (i in (1:length(aves))) {
    p <- p %>%  plotly::add_markers(
      data = port[i,],
      x = ~ Risk,
      y = ~ Return,
      marker = list(size = 12, color = "red")
    )
  }

  p <-  p %>% plotly::colorbar(title = "Sharpe Ratio")

  out <- list(
    "data" = port,
    "plot" = p
  )
  return(out)
}
