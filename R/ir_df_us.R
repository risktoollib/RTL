#' Extracts US Treasury Zero Rates
#' @description Extracts US Treasury Zero Rates curve
#' @param quandlkey Your Quandl key "quandlkey"
#' @param ir.sens Creates plus and minus IR sensitivity scenarios with specified shock value.
#' @return Data frame of zero rates
#' @export ir_df_us
#' @author Philippe Cote
#' @examples
#' \dontrun{
#' us.df <- ir_df_us(quandlkey = quandlkey, ir.sens = 0.01)
#' }
#'
ir_df_us <- function(quandlkey = quandlkey, ir.sens = 0.01) {
  if (!requireNamespace("Quandl", quietly = TRUE)) {
    stop("Package \"Quandl\" needed for this function to work. Please install it.",
      call. = FALSE
    )
  }
  Quandl::Quandl.api_key(quandlkey)
  fedsfund <- tidyquant::tq_get("FED/RIFSPFF_N_D", get = "quandl", from = Sys.Date() - 30) %>%
    stats::na.omit() %>%
    dplyr::transmute(date = date, FedsFunds0 = log((1 + value / (360))^365))
  zero_1yrplus <- Quandl::Quandl("FED/SVENY") %>% dplyr::rename(date = Date)
  zero_tb <- c("FED/RIFLGFCM01_N_B", "FED/RIFLGFCM03_N_B", "FED/RIFLGFCM06_N_B") %>%
    tidyquant::tq_get(get = "quandl", from = Sys.Date() - 30) %>%
    stats::na.omit() %>%
    tidyr::pivot_wider(names_from = symbol, values_from = value)

  x <- dplyr::inner_join(fedsfund, zero_tb, c("date")) %>% dplyr::inner_join(zero_1yrplus, c("date"))
  x <- x %>%
    dplyr::filter(date == dplyr::last(date)) %>%
    dplyr::select(-date)
  x <- x %>%
    t() %>%
    as.data.frame() %>%
    dplyr::transmute(yield = V1 / 100, maturity = readr::parse_number(colnames(x)))
  x$maturity[1] <- 1 / 365
  x$maturity[2:4] <- x$maturity[2:4] / 12
  x <- dplyr::add_row(x, yield = x$yield[1], maturity = 0, .before = 1)
  x <- x %>% dplyr::mutate(
    discountfactor = exp(-yield * maturity),
    discountfactor_plus = exp(-(yield + ir.sens) * maturity),
    discountfactor_minus = exp(-(yield - ir.sens) * maturity)
  )
  return(x)
}
