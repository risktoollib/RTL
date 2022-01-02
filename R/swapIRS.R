#' Interest Rate Swap
#' @description Computes the mark to market of an IRS
#' @param trade.date Date object. Defaults to today().
#' @param eff.date Date object. Defaults to today() + 2 days.
#' @param mat.date Date object. Defaults to today() + 2 years.
#' @param notional Numeric value of notional. Defaults to 1,000,000.
#' @param PayRec "Pay" or "Rec" fixed.
#' @param fixed.rate Numeric fixed interest rate. Defaults to 0.05.
#' @param float.curve List of interest rate curves. Defaults to data("usSwapCurves").
#' @param reset.freq Numeric where 1 = "monthly", 3 = quarterly, 6 = Semi annual 12 = yearly.
#' @param disc.curve List of interest rate curves. Defaults to data("usSwapCurves").
#' @param convention Vector of convention e.g. c("act",360) c(30,360),...
#' @param bus.calendar Banking day calendar. Not implemented.
#' @param output "price" for swap price or "all" for price, cash flow data frame, duration.
#' @return List of swap price, cash flow data frame, duration.
#' @export swapIRS
#' @author Philippe Cote
#' @examples
#' data("usSwapCurves")
#' swapIRS(
#'   trade.date = as.Date("2020-01-04"), eff.date = as.Date("2020-01-06"),
#'   mat.date = as.Date("2022-01-06"), notional = 1000000,
#'   PayRec = "Rec", fixed.rate = 0.05, float.curve = usSwapCurves, reset.freq = 3,
#'   disc.curve = usSwapCurves, convention = c("act", 360),
#'   bus.calendar = "NY", output = "all"
#' )
swapIRS <- function(trade.date = lubridate::today(),
                    eff.date = lubridate::today() + 2,
                    mat.date = lubridate::today() + 2 + lubridate::years(2),
                    notional = 1000000,
                    PayRec = "Rec",
                    fixed.rate = 0.05,
                    float.curve = usSwapCurves,
                    reset.freq = 3,
                    disc.curve = usSwapCurves,
                    convention = c("act", 360),
                    bus.calendar = "NY",
                    output = "price") {
  if (reset.freq == 1) {
    dates <- c(trade.date, seq(eff.date, mat.date, "month"))
  }
  if (reset.freq == 3) {
    dates <- c(trade.date, seq(eff.date, mat.date, "quarter"))
  }
  if (reset.freq == 12) {
    dates <- c(trade.date, seq(eff.date, mat.date, "year"))
  }

  DaysYear <- as.numeric(convention[2])
  if (!DaysYear %in% c(360, 365)) {
    stop("# days in year convention not defined")
  }
  if (convention[1] != "act") {
    stop("function only defined for act")
  }

  tmp <- dplyr::tibble(
    dates = dates,
    day2next = as.numeric(dplyr::lead(dates) - dates),
    times = as.numeric(dates - trade.date) / 365,
    disc = stats::spline(disc.curve$times, disc.curve$discounts, xout = times)$y,
    # condition needed as no payment between trade and effective date
    # lag() needed as payment is in arrear
    fixed = dplyr::lag(ifelse(day2next > 20, notional * fixed.rate * (day2next / DaysYear), 0)) * disc,
    disc.float = stats::spline(float.curve$times, float.curve$discounts, xout = times)$y,
    floating = dplyr::lag(ifelse(day2next > 20, ((disc.float / dplyr::lead(disc.float)) - 1) * notional, 0)) * disc,
    net = fixed - floating
  ) %>% replace(., is.na(.), 0)

  pv <- sum(tmp$net)

  tmp <- tmp %>% dplyr::mutate(duration = as.numeric(dates - trade.date) / DaysYear * net / pv)

  duration <- sum(tmp$duration)

  if (PayRec == "Pay") {
    pv <- -pv
  }

  ifelse(output == "price",
    return(pv),
    return(list(pv, tmp, duration))
  )
}
