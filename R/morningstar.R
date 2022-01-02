#' Morningstar Commodities API single call
#' @description
#' Returns data from Morningstar API. See below for current feeds supported.
#' You need your own credentials with Morningstar. In examples sourced locally.
#'
#' @section Current Feeds Supported:
#' \itemize{
#'   \item CME_CbotFuturesEOD and CME_CbotFuturesEOD_continuous
#'   \item CME_NymexFutures_EOD and CME_NymexFutures_EOD_continuous
#'   \item CME_NymexOptions_EOD
#'   \item CME_CmeFutures_EOD and CME_CmeFutures_EOD_continuous
#'   \item CME_Comex_FuturesSettlement_EOD and CME_Comex_FuturesSettlement_EOD_continuous
#'   \item LME_AskBidPrices_Delayed
#'   \item SHFE_FuturesSettlement_RT
#'   \item ICE_EuroFutures and ICE_EuroFutures_continuous
#'   \item ICE_NybotCoffeeSugarCocoaFutures and ICE_NybotCoffeeSugarCocoaFutures_continuous
#'   \item CME_STLCPC_Futures
#'   \item CFTC_CommitmentsOfTradersCombined. Requires multiple keys. Separate them by a space e.g. "N10 06765A NYME 01".
#'   \item Morningstar_FX_Forwards. Requires multiple keys. Separate them by a space e.g. "USDCAD 2M".
#'   \item ERCOT_LmpsByResourceNodeAndElectricalBus.
#'   \item PJM_Rt_Hourly_Lmp.
#'   \item AESO_ForecastAndActualPoolPrice.
#' }
#'
#' @param feed Morningstar Feed Table.
#' @param contract Morningstar key.
#' @param from From date as character string
#' @param iuser Morningstar user name as character - sourced locally in examples.
#' @param ipassword Morningstar user password as character - sourced locally in examples.
#' @return wide data frame
#' @export getPrice
#' @author Philippe Cote
#' @examples
#' \dontrun{
#' getPrice(
#'   feed = "CME_NymexFutures_EOD", contract = "@CL21Z",
#'   from = "2019-08-26", iuser = username, ipassword = password
#' )
#' getPrice(
#'   feed = "CME_NymexFutures_EOD_continuous", contract = "CL_006_Month",
#'   from = "2019-08-26", iuser = username, ipassword = password
#' )
#' getPrice(
#'   feed = "CME_NymexOptions_EOD", contract = "@LO21ZP4000",
#'   from = "2020-03-15", iuser = username, ipassword = password
#' )
#' getPrice(
#'   feed = "CME_CbotFuturesEOD", contract = "C0Z",
#'   from = "2019-08-26", iuser = username, ipassword = password
#' )
#' getPrice(
#'   feed = "CME_CbotFuturesEOD_continuous", contract = "ZB_001_Month",
#'   from = "2019-08-26", iuser = username, ipassword = password
#' )
#' getPrice(
#'   feed = "CME_CmeFutures_EOD_continuous", contract = "HE_006_Month",
#'   from = "2019-08-26", iuser = username, ipassword = password
#' )
#' getPrice(
#'   feed = "Morningstar_FX_Forwards", contract = "USDCAD 2M",
#'   from = "2019-08-26", iuser = username, ipassword = password
#' )
#' getPrice(
#'   feed = "CME_CmeFutures_EOD", contract = "LH0N",
#'   from = "2019-08-26", iuser = username, ipassword = password
#' )
#' getPrice(
#'   feed = "CME_CmeFutures_EOD_continuous", contract = "HE_006_Month",
#'   from = "2019-08-26", iuser = username, ipassword = password
#' )
#' getPrice(
#'   feed = "ICE_EuroFutures", contract = "BRN0Z",
#'   from = "2019-08-26", iuser = username, ipassword = password
#' )
#' getPrice(
#'   feed = "ICE_EuroFutures_continuous", contract = "BRN_001_Month",
#'   from = "2019-08-26", iuser = username, ipassword = password
#' )
#' getPrice(
#'   feed = "ICE_NybotCoffeeSugarCocoaFutures", contract = "SB21H",
#'   from = "2019-08-26", iuser = username, ipassword = password
#' )
#' getPrice(
#'   feed = "ICE_NybotCoffeeSugarCocoaFutures_continuous", contract = "SF_001_Month",
#'   from = "2019-08-26", iuser = username, ipassword = password
#' )
#' getPrice(
#'   feed = "AESO_ForecastAndActualPoolPrice", contract = "Forecast_Pool_Price",
#'   from = "2021-04-01", iuser = username, ipassword = password
#' )
#' getPrice(
#'   feed = "LME_MonthlyDelayed_Derived", contract = "AHD 2021-12-01 2021-12-31",
#'   from = "2021-04-01", iuser = username, ipassword = password
#' )
#' }
#'
getPrice <- function(feed = "CME_NymexFutures_EOD", contract = "@CL21Z",
                     from = "2020-09-01", iuser = "x@xyz.com", ipassword = "pass") {
  # mpurl <- "https://mp.morningstarcommodity.com/lds/feeds/CME_NymexFutures_EOD/ts?Symbol=@CL9Z"
  # userpw <- paste0(iuser,":",ipassword)
  if (feed %in% c(
    "CME_NymexFutures_EOD", "CME_NymexOptions_EOD", "CME_CbotFuturesEOD", "CME_CmeFutures_EOD",
    "ICE_EuroFutures", "ICE_NybotCoffeeSugarCocoaFutures",
    "CME_Comex_FuturesSettlement_EOD",
    "LME_AskBidPrices_Delayed", "SHFE_FuturesSettlement_RT"
  )) {
    URL <- httr::modify_url(url = "https://mp.morningstarcommodity.com", path = paste0("/lds/feeds/", feed, "/ts?", "Symbol=", contract, "&fromDateTime=", from))
  }
  if (feed %in% c("LME_MonthlyDelayed_Derived")) {
    URL <- httr::modify_url(url = "https://mp.morningstarcommodity.com", path = paste0("/lds/feeds/", feed, "/ts?", "Root=", stringr::word(contract, 1, 1), "&DeliveryStart=", stringr::word(contract, 2, 2), "&DeliveryEnd=", stringr::word(contract, 3, 3), "&fromDateTime=", from))
  }
  if (feed %in% c(
    "CME_NymexFutures_EOD_continuous", "CME_CmeFutures_EOD_continuous",
    "ICE_EuroFutures_continuous", "ICE_NybotCoffeeSugarCocoaFutures_continuous",
    "CME_Comex_FuturesSettlement_EOD_continuous", "CME_CbotFuturesEOD_continuous"
  )) {
    URL <- httr::modify_url(url = "https://mp.morningstarcommodity.com", path = paste0("/lds/feeds/", feed, "/ts?", "Contract=", contract, "&fromDateTime=", from))
  }
  if (feed %in% c("CME_STLCPC_Futures")) {
    URL <- httr::modify_url(url = "https://mp.morningstarcommodity.com", path = paste0("/lds/feeds/", feed, "/ts?", "product=", contract, "&fromDateTime=", from))
  }
  if (feed %in% c("CFTC_CommitmentsOfTradersCombined")) {
    if (grepl(",", contract)) stop(paste("Use a space instead of a comma to separate contract components e.g.", gsub(",", " ", contract)))
    URL <- httr::modify_url(
      url = "https://mp.morningstarcommodity.com",
      path = paste0(
        "/lds/feeds/", feed, "/ts?",
        "cftc_subgroup_code=", stringr::word(contract, 1, 1),
        "&cftc_contract_market_code=", stringr::word(contract, 2, 2),
        "&cftc_market_code=", stringr::word(contract, 3, 3),
        "&cftc_region_code=", stringr::word(contract, 4, 4),
        "&cols=", stringr::word(contract, 5, 5),
        "&fromDateTime=", from
      )
    )
  }

  if (feed == "Morningstar_FX_Forwards") {
    if (grepl(",", contract)) stop(paste("Use a space instead of a comma to separate contract components e.g.", gsub(",", " ", contract)))
    x1 <- stringr::word(contract, 1, 1)
    x2 <- stringr::word(contract, 2, 2)
    URL <- httr::modify_url(
      url = "https://mp.morningstarcommodity.com",
      path = paste0(
        "/lds/feeds/", feed, "/ts?", "cross_currencies=", x1,
        "&period=", x2, "&fromDateTime=", from
      )
    )
  }
  if (feed %in% c("ERCOT_LmpsByResourceNodeAndElectricalBus")) {
    URL <- httr::modify_url(url = "https://mp.morningstarcommodity.com", path = paste0("/lds/feeds/", feed, "/ts?", "SettlementPoint=", contract, "&fromDateTime=", from))
  }
  if (feed %in% c("PJM_Rt_Hourly_Lmp")) {
    URL <- httr::modify_url(url = "https://mp.morningstarcommodity.com", path = paste0("/lds/feeds/", feed, "/ts?", "pnodeid=", contract, "&fromDateTime=", from))
  }
  if (feed %in% c("AESO_ForecastAndActualPoolPrice")) {
    URL <- httr::modify_url(url = "https://mp.morningstarcommodity.com", path = paste0("/lds/feeds/", feed, "/ts?", "market=", contract, "&fromDateTime=", from))
  }

  httr::handle_reset(URL)
  es <- httr::GET(url = URL, httr::authenticate(user = iuser, password = ipassword, type = "basic")) # ,httr::progress())
  es <- httr::content(es)
  elecFeeds <- "ERCOT|PJM|AESO"
  # Non electricity feeds
  if (!grepl(elecFeeds, feed) & length(es) > 0) {
    if (length(es %>% purrr::flatten() %>% .$series %>% .$values %>% purrr::flatten()) > 0) {
      out <-
        dplyr::tibble(
          date = as.character(lubridate::ymd(es %>% purrr::flatten() %>% purrr::flatten() %>% .$dates)) %>% lubridate::ymd(),
          opt = feed == "CME_NymexOptions_EOD",
          value = ifelse(opt == TRUE,
            as.numeric(es %>% purrr::flatten() %>% purrr::flatten() %>% .$values %>% .[[4]] %>% purrr::flatten()),
            as.numeric(es %>% purrr::flatten() %>% purrr::flatten() %>% .$values %>% .[[1]] %>% purrr::flatten())
          )
        ) %>%
        dplyr::select(-opt) %>%
        dplyr::mutate(value = ifelse(is.nan(value), NA, value))
    } else {
      out <- dplyr::tibble(
        date = character(),
        value = numeric(),
        fwdmnt = numeric(),
        fwdyr = numeric()
      )
    }
  }
  # electricy feeds
  if (grepl(elecFeeds, feed) & length(es) > 0) {
    if (length(es %>% purrr::flatten() %>% .$series %>% .$values %>% purrr::flatten()) > 0) {
      if (grepl("ERCOT", feed)) {
        tz <- "CST"
        x <- 1
      }
      if (grepl("PJM", feed)) {
        tz <- "EST"
        x <- 5
      }
      if (grepl("AESO", feed)) {
        tz <- "MST"
        x <- 1
      }
      out <-
        dplyr::tibble(
          date = as.POSIXct(sub("T", "", es %>% purrr::flatten() %>% purrr::flatten() %>% .$date %>% unlist()), tz = tz),
          value = as.numeric(es %>% purrr::flatten() %>% purrr::flatten() %>% .$values %>% .[[x]] %>% purrr::flatten())
        ) %>%
        dplyr::mutate(value = ifelse(is.nan(value), NA, value))
    } else {
      out <- dplyr::tibble(
        date = character(),
        value = numeric(),
        fwdmnt = numeric(),
        fwdyr = numeric()
      )
    }
  }
  if (length(colnames(out)) == 2) {
    colnames(out)[2] <- sub("@", "", contract)
  }
  if (feed %in% c("LME_MonthlyDelayed_Derived")) {
    colnames(out)[2] <- gsub("-", "", paste0(stringr::word(contract, 1, 1), substr(stringr::word(contract, 2, 2), 1, 7)))
  }

  return(out)
}

# getPrice2(feed = "CME_NymexFutures_EOD",contract = "@CL21Z",from = "2020-09-01",iuser, ipassword )
# getPrice2(feed = "ERCOT_LmpsByResourceNodeAndElectricalBus",contract = "HB_NORTH",from = "2020-09-01",iuser, ipassword )
# getPrice2(feed = "PJM_Rt_Hourly_Lmp",contract = "51287",from = "2020-09-01",iuser, ipassword )
# getPrice2(feed = "AESO_ForecastAndActualPoolPrice",contract = "Forecast_Pool_Price",from,iuser,ipassword)


#' Morningstar Commodities API multiple calls
#' @description
#' Multiple Morningstar API calls using getPrice functions.
#' Refer to `getPrices()` for list of currently supported data feeds.
#' @param feed Morningstar Feed Table
#' @param contracts Symbols vector
#' @param from From date as character string
#' @param iuser Morningstar user name as character - sourced locally in examples.
#' @param ipassword Morningstar user password as character - sourced locally in examples.
#' @return wide data frame
#' @export getPrices
#' @author Philippe Cote
#' @examples
#' \dontrun{
#' getPrices(
#'   feed = "CME_NymexFutures_EOD", contracts = c("@CL0Z", "@CL1F", "@CL21H", "@CL21Z"),
#'   from = "2020-01-01", iuser = username, ipassword = password
#' )
#' }
#'
getPrices <- function(feed = "CME_NymexFutures_EOD", contracts = c("CL9Z", "CL0F", "CL0M"), from = "2019-01-01", iuser = "x@xyz.com", ipassword = "pass") {
  x <- getPrice(feed = feed, contract = contracts[1], from = from, iuser = iuser, ipassword = ipassword)
  for (c in contracts[-1]) {
    x <- merge(x,
      getPrice(feed = feed, contract = c, from = from, iuser = iuser, ipassword = ipassword),
      all = TRUE
    )
  }
  x <- dplyr::as_tibble(x)
  return(x)
}

#' Morningstar Commodities API single call for IR curves
#' @description
#' Extract historical data for tsQuotes in RQuantlib to bootstrap swap curve
#' using Morningstar and FRED as data source.
#' @param currency Currently only USD LIBOR implemented.
#' @param from From date as character string
#' @param iuser Morningstar user name as character - sourced locally in examples.
#' @param ipassword Morningstar user password as character - sourced locally in examples.
#' @return wide data frame
#' @export getIRswapCurve
#' @author Philippe Cote
#' @examples
#' \dontrun{
#' getIRswapCurve(currency = "USD", from = "2019-08-26", iuser = username, ipassword = password)
#' }
#'
getIRswapCurve <- function(currency = "USD", from = "2019-01-01", iuser = "x@xyz.com", ipassword = "pass") {
  usSwapIR <- dplyr::tibble(
    tickQL = c(
      "d1d", "d1w", "d1m", "d3m", "d6m", "d1y",
      paste0("fut", 1:8),
      paste0("s", c(2, 3, 5, 7, 10, 15, 20, 30), "y")
    ),
    type = c(rep("ICE.LIBOR", 6), rep("EuroDollar", 8), rep("IRS", 8)),
    source = c(rep("FRED", 6), rep("Morningstar", 8), rep("FRED", 8)),
    tickSource = c(
      "USDONTD156N", "USD1WKD156N", "USD1MTD156N", "USD3MTD156N", "USD6MTD156N", "USD12MD156N",
      paste0("ED_", sprintf("%0.3d", 1:8), "_Month"),
      paste0("ICERATES1100USD", c(2, 3, 5, 7, 10, 15, 20, 30), "Y")
    )
  )

  c <- usSwapIR %>%
    dplyr::filter(source == "Morningstar") %>%
    .$tickSource
  r <- getPrices(feed = "CME_CmeFutures_EOD_continuous", contracts = c, from = from, iuser = iuser, ipassword = ipassword)
  c <- usSwapIR %>%
    dplyr::filter(source == "FRED") %>%
    .$tickSource
  x <- tidyquant::tq_get(c, get = "economic.data", from = from, to = as.character(Sys.Date())) %>%
    dplyr::mutate(price = price / 100) %>%
    tidyr::pivot_wider(date, names_from = symbol, values_from = price)
  r <- dplyr::left_join(x, r, by = c("date"))
  colnames(r) <- c("date", dplyr::tibble(tickSource = colnames(r)[-1]) %>% dplyr::left_join(usSwapIR, by = c("tickSource")) %>% .$tickQL)
  return(r)
}


#' Morningstar Commodities API forward curves
#' @description
#' Returns forward curves from Morningstar API. See below for current feeds supported.
#' You need your own credentials with Morningstar.
#'
#' @section Current Feeds Supported:
#' \itemize{
#'   \item Crb_Futures_Price_Volume_And_Open_Interest
#'   \item CME_NymexFuturesIntraday_EOD
#'   \item ICE_EuroFutures and ICE_EuroFutures_continuous
#' }
#'
#' @param feed Morningstar Feed Table e.g "Crb_Futures_Price_Volume_And_Open_Interest".
#' @param contract Morningstar contract root e.g. "CL" for CME WTI and "BG" for ICE Brent.
#' @param date From date as character string.
#' @param fields Defaults to c("Open, High, Low, Close").
#' @param iuser Morningstar user name as character - sourced locally in examples.
#' @param ipassword Morningstar user password as character - sourced locally in examples.
#' @return wide data frame
#' @export getCurve
#' @author Philippe Cote
#' @examples
#' \dontrun{
#' # CME WTI Futures
#' getCurve(
#'   feed = "Crb_Futures_Price_Volume_And_Open_Interest", contract = "CL",
#'   date = "2020-07-13", fields = c("Open, High, Low, Close"),
#'   iuser = "x@xyz.com", ipassword = "pass"
#' )
#'
#' getCurve(
#'   feed = "Crb_Futures_Price_Volume_And_Open_Interest", contract = "BG",
#'   date = "2020-07-13", fields = c("Open, High, Low, Close"),
#'   iuser = "x@xyz.com", ipassword = "pass"
#' )
#'
#' getCurve(
#'   feed = "LME_ClosingPriceDelayed", contract = "AHD",
#'   date = "2021-06-25", fields = c("Last_Price"),
#'   iuser = "x@xyz.com", ipassword = "pass"
#' )
#' }
#'
getCurve <- function(feed = "Crb_Futures_Price_Volume_And_Open_Interest", contract = "CL", date = "2020-08-10",
                     fields = c("Open, High, Low, Close"),
                     iuser = "x@xyz.com", ipassword = "pass") {
  URL <- httr::modify_url(
    url = "https://mp.morningstarcommodity.com",
    path = paste0(
      "/lds/feeds/", feed, "/curve?root=", contract, "&cols=", gsub(" ", "", fields),
      "&date=", date
    )
  )
  es <- RCurl::getURL(url = URL, userpw = paste(iuser, ipassword, sep = ":"))
  out <- jsonlite::fromJSON(es) %>%
    dplyr::as_tibble() %>%
    dplyr::arrange(deliveryStartDate)

  if (grepl("LME_MonthlyDelayed_Derived", feed)) {
    out <- out %>% dplyr::mutate(expirationDate = lubridate::rollback(as.Date(deliveryStartDate), roll_to_first = T) - 1)
    es <- NA
  } else {
    es <- out$keys %>%
      unlist() %>%
      unique()
  }

  out <- out %>%
    dplyr::transmute(
      expirationDate = as.Date(expirationDate),
      type = col,
      value = as.numeric(value)
    ) %>%
    tidyr::pivot_wider(names_from = type, values_from = value) %>%
    dplyr::arrange(expirationDate)

  out <- out %>%
    dplyr::mutate(
      contract = paste(contract, sprintf("%0.2d", 1:nrow(out)), sep = ""),
      code = es
    ) %>%
    dplyr::select(contract, code, dplyr::everything())
  return(out)
}

#  getCurve(feed="Crb_Futures_Price_Volume_And_Open_Interest",contract="BG",date = "2020-07-10",
#           fields = c("Open, High, Low, Close"),
#          iuser = mstar[[1]], ipassword = mstar[[2]])

# getCurve <- function(feed = "Crb_Futures_Price_Volume_And_Open_Interest",contract = "CL",date ="2020-07-13",
#                      fields = c("Open, High, Low, Close"),
#                      iuser = "x@xyz.com", ipassword = "pass") {
#
#   URL = httr::modify_url(url = "https://mp.morningstarcommodity.com",
#                          path = paste0("/lds/feeds/",feed, "/curve?root=",contract,"&cols=",gsub(" ","",fields),
#                                        "&date=",date))
#   httr::handle_reset(URL)
#   es <- httr::GET(url = URL,httr::authenticate(user = iuser,password = ipassword,type = "basic"))
#   out <- es %>% httr::content()
#   out <- tibble::tibble(purrr::map(out,"expirationDate") %>% unlist() %>% tibble::enframe(name = NULL, value = "expiry") %>% dplyr::transmute(expiry = as.Date(expiry)),
#                         purrr::map(out,"col") %>% unlist() %>% tibble::enframe(name = NULL, value = "type"),
#                         purrr::map(out,"value") %>% unlist() %>% tibble::enframe(name = NULL)) %>%
#     dplyr::arrange(expiry) %>%
#     tidyr::pivot_wider(names_from = type, values_from = value)
#   return(out)
# }
