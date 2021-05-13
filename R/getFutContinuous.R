#' \code{getFutContinuous}
#' @description Extracts Continuous Futures Contracts from Yahoo Finance.
#' @section Contract root code on Yahoo Finance examples:
#' \itemize{
#'   \item CL = CME WTI
#'   \item HO = CME ULSD
#'   \item RB = CME RBOB
#'   \item BZ = ICE Brent
#'   \item ...
#' }
#' @param roots Contract root codes on Yahoo Finance eg c("CL"), c("CL","HO")
#' @param contracts Contracts out eg. c(1:10)
#' @param from Date as character string or Date class
#' @return An OHLC long data frame
#' @export getFutContinuous
#' @author Mikel Buchinski, Joe Rikk, Usman Farooq, Gabriela Sanchez
#' @examples
#' getFutContinuous(roots = c("CL"), contracts = c(1:3), from = "2021-01-01")

getFutContinuous <- function(roots = c("CL"), contracts = c(1:3),from = Sys.Date()-365) {
  tick <- c("CLF22.NYM")
  out <- tidyquant::tq_get(tick,get = "stock.prices", from = from) %>%
    dplyr::select(-adjusted)
  return(out)
}

