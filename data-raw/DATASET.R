# usethis::use_cran_badge()
# usethis::use_pipe()
# usethis::use_readme_md()
# attachment::att_from_rscripts()
# usethis::use_package("lpSolve", "suggests")
# usethis::use_package("rugarch", "suggests")
# usethis::use_package("sf","suggests")
# usethis::use_package("PerformanceAnalytics","suggests")
# usethis::use_package("tidyquant","suggests")
# usethis::use_package("feasts","suggests")
# usethis::use_package("fabletools","suggests")
# usethis::use_package("MASS","suggests")
# usethis::use_package("dplyr")
# usethis::use_package("stringr")
# usethis::use_package("tibble")
# usethis::use_package("tidyr")
# usethis::use_package("plotly")
# usethis::use_package("lubridate")
# usethis::use_package("xts")
# usethis::use_package("readr")
# usethis::use_package("ggplot2")
# usethis::use_package("tsibble")
# usethis::use_package("httr")
# usethis::use_package("jsonlite")
# usethis::use_package("timetk")
# usethis::use_package("magrittr")
# usethis::use_package("rlang")
# usethis::use_package("RCurl")
# usethis::use_package("purrr")
# usethis::use_package("zoo")
# usethis::use_package("TTR")
# usethis::use_package("Rcpp")
# usethis::use_rcpp()
# usethis::use_package("tidyselect")
# usethis::use_package("PerformanceAnalytics")
#usethis::use_github_action("check-standard")
spelling::spell_check_package()
spelling::update_wordlist()
devtools::document()
# usethis::use_github_links()
# Setup RTL Webpage
# usethis::use_pkgdown()
# usethis::use_github_actions("pkgdown")
# usethis::use_tidy_github_actions()
# usethis::use_tidy_description()
# usethis::use_tidy_dependencies()
# usethis::use_roxygen_md()
# pkgdown::build_site()
# usethis::use_github_action(url = "https://raw.githubusercontent.com/r-lib/actions/master/examples/pkgdown.yaml")
#usethis::use_testthat()
devtools::check_win_release()
devtools::check_win_devel()
devtools::check_mac_release()

#
library(RTL)
library(curl)
library(tidyverse)
library(lubridate)
library(jsonlite)
library(rvest)
library(readxl)
library(readr)
library(RSelenium)
source("~/now/packages.R")
setwd(paste0(getwd(), "/data-raw"))




# Futures Metadata --------------------------------------------------------
futuresRef <- list()
futuresRef$ContractMonths <-
  dplyr::tibble(
    Month = seq.Date(as.Date("2022-01-01"), as.Date("2022-12-01"), by = "months") %>% lubridate::month(label = TRUE, abbr = FALSE) %>% as.character(.),
    Code = c("F", "G", "H", "J", "K", "M", "N", "Q", "U", "V", "X", "Z")
  )
  # Futures specifications
futuresRef$Specifications <- list()

futuresRef$Specifications$CL <-
  system(command = "node ../inst/js/cme-specifications.js https://www.cmegroup.com/markets/energy/crude-oil/light-sweet-crude.contractSpecs.html",
       intern = TRUE) %>%
  jsonlite::fromJSON()  %>%
  dplyr::as_tibble()

futuresRef$Specifications$HH <-
  system(command = "node ../inst/js/cme-specifications.js https://www.cmegroup.com/markets/energy/natural-gas/natural-gas.contractSpecs.html",
         intern = TRUE) %>%
  jsonlite::fromJSON()  %>%
  dplyr::as_tibble()

futuresRef$Specifications$CS <-
  system(command = "node ../inst/js/cme-specifications.js https://www.cmegroup.com/markets/energy/crude-oil/west-texas-intermediate-wti-crude-oil-calendar-swap-futures.contractSpecs.html",
         intern = TRUE) %>%
  jsonlite::fromJSON()  %>%
  dplyr::as_tibble()

futuresRef$Specifications$RB <-
  system(command = "node ../inst/js/cme-specifications.js https://www.cmegroup.com/markets/energy/refined-products/rbob-gasoline.contractSpecs.html",
         intern = TRUE) %>%
  jsonlite::fromJSON()  %>%
  dplyr::as_tibble()

futuresRef$Specifications$HO <-
  system(command = "node ../inst/js/cme-specifications.js https://www.cmegroup.com/markets/energy/refined-products/heating-oil.contractSpecs.html",
         intern = TRUE) %>%
  jsonlite::fromJSON()  %>%
  dplyr::as_tibble()

futuresRef$Specifications$HTT <-
  system(command = "node ../inst/js/cme-specifications.js https://www.cmegroup.com/markets/energy/crude-oil/wti-houston-argus-vs-wti-trade-month.contractSpecs.html",
         intern = TRUE) %>%
  jsonlite::fromJSON()  %>%
  dplyr::as_tibble()

futuresRef$Specifications$ZN <-
  system(command = "node ../inst/js/cme-specifications.js https://www.cmegroup.com/markets/interest-rates/us-treasury/10-year-us-treasury-note.contractSpecs.html",
         intern = TRUE) %>%
  jsonlite::fromJSON()  %>%
  dplyr::as_tibble()

futuresRef$Specifications$SOFR <-
  system(command = "node ../inst/js/cme-specifications.js https://www.cmegroup.com/markets/interest-rates/stirs/three-month-sofr.contractSpecs.html",
         intern = TRUE) %>%
  jsonlite::fromJSON()  %>%
  dplyr::as_tibble()

usethis::use_data(futuresRef, overwrite = T)

# Equities ----------------------------------------------------------------

stocks <- list()

stocks$spy <-
  tidyquant::tq_get("SPY") %>%
  dplyr::mutate(ret = log(adjusted / dplyr::lag(adjusted))) %>%
  stats::na.omit() %>%
  dplyr::select(date, ret)

stocks$uso <- tidyquant::tq_get("USO", adjust = TRUE) %>%
  dplyr::rename_all(tools::toTitleCase) %>%
  timetk::tk_xts(date_var = Date) %>%
  quantmod::adjustOHLC(., use.Adjusted = TRUE) %>%
  timetk::tk_tbl(rename_index = "Date") %>%
  dplyr::select(-Adjusted) %>%
  dplyr::mutate(across(where(is.numeric), round, digits = 2))


ry <- tidyquant::tq_get("RY", adjust = TRUE, from = "2000-01-01") %>%
  dplyr::rename_all(tools::toTitleCase) %>%
  timetk::tk_xts(date_var = Date) %>%
  quantmod::adjustOHLC(.,use.Adjusted = TRUE) %>%
  timetk::tk_tbl(rename_index = "Date") %>%
  dplyr::select(-Adjusted) %>%
  dplyr::mutate(across(where(is.numeric), round, digits = 2))

dividends <- tidyquant::tq_get("RY", get = "dividends" , from = "2000-01-01",adjust = TRUE)
stocks$ry <- ry %>%
  dplyr::left_join(dividends %>% dplyr::transmute(Date = date, Dividend = value), by = c("Date")) %>%
  tidyr::fill(Dividend) %>%
  tidyr::drop_na() %>%
  dplyr::mutate(Dividend = (1 + Dividend / Close)^4-1)

usethis::use_data(stocks, overwrite = T)

# Commodities ----------------------------------------------------------------

## spot2fut convergence
d <- "2020-03-25"
tick = "PET.RWTC.D"
cash <-
  RTL::eia2tidy(ticker = tick, key = EIAkey, name = "cash") %>%
  dplyr::arrange(date)
c <- cash %>% dplyr::filter(date == d)
f <-
  RTL::getCurve(
    feed = "Crb_Futures_Price_Volume_And_Open_Interest",
    contract = "CL",
    date = d,
    fields = c("Open, High, Low, Close"),
    iuser = mstar[[1]],
    ipassword = mstar[[2]]
  ) %>%
  dplyr::slice(1:12) %>%
  dplyr::select(-Open, -High, -Low)

spot2futCurve <- f %>%
  dplyr::add_row(
    contract = "cash",
    code = tick,
    expirationDate = c$date,
    Close = c$cash
  ) %>%
  dplyr::arrange(contract)
usethis::use_data(spot2futCurve, overwrite = T)

fut <- RTL::getPrice(
  feed = "CME_NymexFutures_EOD",
  contract = "CL0M",
  from = "2019-01-01",
  iuser = mstar[[1]],
  ipassword = mstar[[2]]
)
colnames(fut)[2] <- "fut"

spot2futConvergence <- fut %>%
  dplyr::inner_join(cash, by = c("date")) %>%
  dplyr::mutate(spot2fut = cash - fut) %>%
  tidyr::pivot_longer(-date, names_to = "series", values_to = "value")
usethis::use_data(spot2futConvergence, overwrite = T)

## WTI swaps
c <- paste0("CL0", c("M", "N", "Q"))
wtiSwap <- RTL::getPrices(
  feed = "CME_NymexFutures_EOD",
  contracts = c,
  from = "2019-08-26",
  iuser = mstar[[1]], ipassword = mstar[[2]]
)

usethis::use_data(wtiSwap, overwrite = T)

## Eurodollar

eurodollar <- RTL::getPrices(
  feed = "CME_CmeFutures_EOD",
  contracts = c("ED24Z"),
  from = "2019-01-01",
  iuser = mstar[[1]],
  ipassword = mstar[[2]]
) %>%
  tidyr::pivot_longer(-date, names_to = "series", values_to = "price")

usethis::use_data(eurodollar, overwrite = T)

## Sample energy futures datasets
# df_fut <- readRDS("df_fut") ; usethis::use_data(df_fut, overwrite = T)

iuser <- mstar[["iuser"]]
ipassword <- mstar[["ipassword"]]
startdate <- "2004-01-01"

crude <- c(
  paste0("CL_", sprintf("%0.3d", 1:36), "_Month"),
  paste0("NG_", sprintf("%0.3d", 1:36), "_Month"),
  paste0("HTT_", sprintf("%0.3d", 1:12), "_Month")
  )

crudeICE <- c(paste0("BRN_", sprintf("%0.3d", 1:36), "_Month"))
pdts <- c(paste0("HO_", sprintf("%0.3d", 1:18), "_Month"),
          paste0("RB_", sprintf("%0.3d", 1:18), "_Month"))

crude <- RTL::getPrices(
  feed = "CME_NymexFutures_EOD_continuous",
  contracts = crude,
  from = startdate,
  iuser = iuser,
  ipassword = ipassword
) %>%
  pivot_longer(-date, names_to = "series", values_to = "value") %>%
  dplyr::mutate(series = stringr::str_replace_all(series, c("_0" = "", "_Month" = ""))) %>%
  na.omit()

crudeICE <- RTL::getPrices(
  feed = "ICE_EuroFutures_continuous",
  contracts = crudeICE,
  from = startdate,
  iuser = iuser,
  ipassword = ipassword
) %>%
  pivot_longer(-date, names_to = "series", values_to = "value") %>%
  dplyr::mutate(series = stringr::str_replace_all(series, c("_0" = "", "_Month" = ""))) %>%
  na.omit()

pdts <- RTL::getPrices(
  feed = "CME_NymexFutures_EOD_continuous",
  contracts = pdts,
  from = startdate,
  iuser = iuser,
  ipassword = ipassword
) %>%
  pivot_longer(-date, names_to = "series", values_to = "value") %>%
  dplyr::mutate(series = stringr::str_replace_all(series, c("_0" = "", "_Month" = ""))) %>%
  na.omit()

alu <- c(
  paste("ALI", sprintf(fmt = "%0.3d", 1:6), "Month", sep = "_"),
  paste("AUP", sprintf(fmt = "%0.3d", 1:6), "Month", sep = "_"),
  paste("EDP", sprintf(fmt = "%0.3d", 1:6), "Month", sep = "_"),
  paste("MJP", sprintf(fmt = "%0.3d", 1:6), "Month", sep = "_")
)

lbs2mt <- function(x) {
  x * 55116 / 25
}

alu <-
  RTL::getPrices(
    feed = "CME_Comex_FuturesSettlement_EOD_continuous",
    contracts = alu,
    from = startdate,
    iuser = mstar[[1]],
    ipassword = mstar[[2]]
  ) %>%
  dplyr::rename_all(~ str_replace_all(., "_Month|_0", "")) %>%
  dplyr::mutate(across(dplyr::contains("AUP"), lbs2mt)) %>%
  tidyr::pivot_longer(-date, names_to = "series", values_to = "value")

dateMin = as.Date("2007-01-01")
dflong <- rbind(crude, crudeICE, pdts, alu) %>%
  dplyr::filter(date > dateMin)
dfwide <- dflong %>%
  dplyr::filter(date > dateMin) %>%
  tidyr::pivot_wider(names_from = series, values_from = value) # %>% na.omit()

# test for data gaps
dflong %>% dplyr::filter(grepl("CL",series)) %>% ggplot(aes(x = date, y = value, col = series)) + geom_line()
dflong %>% dplyr::filter(grepl("HTT",series)) %>% ggplot(aes(x = date, y = value, col = series)) + geom_line()
dflong %>% dplyr::filter(grepl("BRN",series)) %>% ggplot(aes(x = date, y = value, col = series)) + geom_line()
dflong %>% dplyr::filter(grepl("HO",series)) %>% ggplot(aes(x = date, y = value, col = series)) + geom_line()
dflong %>% dplyr::filter(grepl("RB",series)) %>% ggplot(aes(x = date, y = value, col = series)) + geom_line()
dflong %>% dplyr::filter(grepl("ALI",series)) %>% ggplot(aes(x = date, y = value, col = series)) + geom_line()
dflong %>% dplyr::filter(grepl("NG",series)) %>% ggplot(aes(x = date, y = value, col = series)) + geom_line()
dflong %>% dplyr::filter(grepl("AUP",series)) %>% ggplot(aes(x = date, y = value, col = series)) + geom_line()
dflong %>% dplyr::filter(grepl("MJP",series)) %>% ggplot(aes(x = date, y = value, col = series)) + geom_line()

usethis::use_data(dflong , overwrite = T)
usethis::use_data(dfwide, overwrite = T)
rm(crude, crudeICE, pdts, alu)

# EIA ---------------------------------------------------------------------

## steo for RTLappWTI

steo <- RTL::chart_eia_steo(key = EIAkey)
usethis::use_data(steo, overwrite = T)

## Sample EIA dataset
eiaStocks <- tibble::tribble(
  ~ticker, ~name,
  "PET.W_EPC0_SAX_YCUOK_MBBL.W", "CrudeCushing",
  "PET.WGTSTP11.W", "Gasoline",
  "PET.WD0ST_R10_1.W", "ULSD",
  "NG.NW2_EPG0_SWO_R48_BCF.W", "NGLower48"
) %>%
  dplyr::mutate(key = EIAkey) %>%
  dplyr::mutate(df = purrr::pmap(list(ticker, key, name), .f = RTL::eia2tidy)) %>%
  dplyr::select(df) %>%
  tidyr::unnest(df) %>%
  tidyr::pivot_longer(-date, names_to = "series", values_to = "value") %>%
  tidyr::drop_na() %>%
  dplyr::group_by(series)
usethis::use_data(eiaStocks, overwrite = T)

## EIA Storage Capacity
### Crude
url <- "https://www.eia.gov/petroleum/storagecapacity/crudeoilstorage.xlsx"
destfile <- "crudeoilstorage.xlsx"
curl::curl_download(url, destfile)
name <- "Refinery and Tank and Underground Working Storage Capacity"

cc <- function(name = "Refinery and Tank and Underground Working Storage Capacity", sheet = "US", loc = "US") {
  tmp <- read_excel(destfile, skip = 3, sheet = sheet) %>%
    dplyr::filter(.[[1]] == name) %>%
    dplyr::rename(series = "...1") %>%
    dplyr::mutate(series = loc)
  colnames(tmp) <- c("series", as.character(as.Date(as.numeric(colnames(tmp)[-1]), origin = "1899-12-30")))
  tmp <- tmp %>%
    tidyr::pivot_longer(-series, names_to = "date", values_to = "value") %>%
    dplyr::mutate(date = as.Date(date), value = as.numeric(value))
  return(tmp)
}

eiaStorageCap <- bind_rows(
  cc(sheet = "US", loc = "US"),
  cc(sheet = "PADD 1", loc = "P1"),
  cc(sheet = "PADD 2", loc = "P2"),
  cc(sheet = "PADD 3", loc = "P3"),
  cc(sheet = "PADD 4", loc = "P4"),
  cc(sheet = "PADD 5", loc = "P5"),
  cc(name = "Tank Working Storage Capacity", sheet = "Cushing", loc = "Cushing")
)

eiaStorageCap <- eiaStorageCap %>% dplyr::mutate(product = "crude")

### Products
library(rvest)
library(pdftools)
library(tesseract)
url <- "https://www.eia.gov/petroleum/storagecapacity/archive/"
urls <- rvest::read_html(url) %>%
  rvest::html_element(css = "table") %>%
  html_elements("tr") %>%
  rvest::html_elements("a") %>%
  html_attr("href") %>%
  paste0("https://www.eia.gov", .)

dist1b <- dplyr::tibble(date = as.Date("2021-01-01"), p1mdist = 0, p1ldist = 0)

for (i in 1:length(urls)) {
  reportDate <- as.Date(stringr::str_sub(urls[i], 60, 69), format = "%Y_%m_%d")
  if (reportDate >= as.Date("2019-01-01")) {
    urli <- urls[i] %>%
      rvest::read_html() %>%
      rvest::html_element(css = "table") %>%
      html_elements("tr") %>%
      rvest::html_elements("a") %>%
      html_attr("href")
    urli <- gsub(pattern = "/[^/]*$", replacement = paste0("/", urli[1]), x = urls[i])
    destfile <- "storagecapacity.xlsx"
    curl::curl_download(urli, destfile)
    storagecapacity <- readxl::read_excel(destfile, sheet = "Table 1", skip = 8)
    dist1b[i, 1] <- reportDate
    dist1b[i, 2] <- (storagecapacity %>% dplyr::filter(.[[1]] == "Distillate Fuel Oil"))[2, 2] %>% as.numeric(.)
    dist1b[i, 3] <- (storagecapacity %>% dplyr::filter(.[[1]] == "Motor Gasoline (incl. Motor Gasoline Blending Components)"))[2, 2] %>% as.numeric(.)
  } else {
    urli <- gsub(pattern = "/[^/]*$", replacement = paste0("/", "table1.pdf"), x = urls[i])
    tmp <- pdftools::pdf_ocr_text(urli, pages = 1)
    tmp <- read_lines(tmp)
    tmp <- str_replace_all(
      string = tmp[grep("^Motor.*|^Dist.*", tmp)],
      pattern = c("Distillate Fuel Oil " = "", "Motor Gasoline \\(incl. Motor Gasoline Blending Components\\) " = "")
    )[3:4]

    # tmp <-  pdf_ocr_data(urli,pages = 1)
    dist1b[i, 1] <- reportDate
    dist1b[i, 2] <- readr::parse_number(sub(" .*", "", tmp))[2]
    dist1b[i, 3] <- readr::parse_number(sub(" .*", "", tmp))[1]
  }
}

dist1b <- dist1b %>%
  dplyr::rename("distillates" = 2, "gasoline" = 3) %>%
  dplyr::mutate(series = "P1") %>%
  tidyr::pivot_longer(cols = c(-date, -series), names_to = "product", values_to = "value") %>%
  dplyr::select(series, date, value, product)

ng <- RTL::eia2tidy(
  ticker = "NG.NGM_EPG0_SACW0_R48_MMCF.M",
  key = EIAkey,
  name = "lower48"
) %>%
  dplyr::transmute(series = "lower48",
                   date,
                   value = lower48 / 1000,
                   product = "ng")

eiaStorageCap <- rbind(eiaStorageCap, dist1b, ng)
file.remove(destfile)
usethis::use_data(eiaStorageCap, overwrite = T)

# cushing dataset

cushing = list()
c2 <- dfwide %>% dplyr::select(date,CL02) %>% tidyr::drop_na()
c1 <- tidyquant::tq_get("CL=F", adjust = TRUE) %>%
  dplyr::rename_all(tools::toTitleCase) %>%
  dplyr::mutate(symbol = "c1") %>%
  timetk::tk_xts(date_var = Date) %>%
  quantmod::adjustOHLC(.,use.Adjusted = TRUE) %>%
  timetk::tk_tbl(rename_index = "Date") %>%
  dplyr::select(-Adjusted, - Volume)  %>%
  dplyr::filter(Date %in% c(c2$date)) %>%
  dplyr::rename(date = Date)

ohlc <- c1 %>%
  dplyr::transmute(date = date,
                   Open = Open / Close,
                   High = High / Close,
                   Low = Low / Close,
                   Close = 1)

c2 <- c2 %>% dplyr::left_join(ohlc) %>% tidyr::drop_na()
c2 <- cbind(date = c2 %>% dplyr::pull(date),c2 %>% dplyr::select(-date,-CL02) * c2$CL02) %>% dplyr::as_tibble()
c1c2 <- cbind(date = c1$date,c1 %>% dplyr::select(-date) - c2 %>% dplyr::select(-date)) %>%
  dplyr::as_tibble()

storage <- rbind(eiaStocks %>% dplyr::filter(series == "CrudeCushing"),
                 eiaStorageCap %>% dplyr::filter(series == "Cushing") %>% dplyr::select(-product))
spreads <- dflong %>%
  dplyr::filter(grepl("CL01|CL02", series)) %>%
  tidyr::pivot_wider(names_from = series, values_from = value) %>%
  dplyr::transmute(date, c1c2 = .[[2]] - .[[3]]) %>%
  tidyr::drop_na()

spreads <- RTL::rolladjust(x = spreads,commodityname = "cmewti",
                           rolltype = c("Last.Trade"))

spreads <- spreads %>% dplyr::filter(abs(c1c2) < 10)

storage <- storage %>%
  tidyr::pivot_wider(names_from = series, values_from = value) %>%
  dplyr::arrange(date) %>%
  dplyr::rename(stocks = 2, capacity = 3) %>%
  tidyr::fill(capacity) %>%
  tidyr::drop_na() %>%
  dplyr::mutate(utilization = stocks / capacity,
                year = lubridate::year(date)) %>%
  dplyr::left_join(spreads %>% dplyr::select(date, c1c2)) %>%
  tidyr::drop_na()

cushing$c1 <- c1
cushing$c2 <- c2
cushing$c1c2 <- c1c2
cushing$storage <- storage
usethis::use_data(cushing, overwrite = T)

## EIA Mapping
tickers_eia <- read.csv("eia.csv", sep = ",", header = TRUE, na.strings = "NA", stringsAsFactors = FALSE)
usethis::use_data(tickers_eia, overwrite = T)

# Calendars ---------------------------------------------------------------

holidaysOil <-
  read.csv(
    "holidays.csv",
    sep = ",",
    header = TRUE,
    na.strings = "NA",
    stringsAsFactors = FALSE
  ) %>%
  dplyr::mutate(
    nymex = as.Date(as.character(nymex), "%Y-%m-%d", tz = "UTC"),
    ice = as.Date(as.character(ice), "%Y-%m-%d", tz = "UTC")
  ) %>%
  tidyr::gather()
holidaysOil <- holidaysOil[complete.cases(holidaysOil), ] %>%
  dplyr::as_tibble() %>%
  dplyr::arrange(key,value)
usethis::use_data(holidaysOil, overwrite = T)

# Expiry table
futmonths <- c("F", "G", "H", "J", "K", "M", "N", "Q", "U", "V", "X", "Z")
expiry_table <- read.csv("expiry_table.csv", sep = ",", header = TRUE, na.strings = "NA", stringsAsFactors = FALSE) %>%
  dplyr::as_tibble() %>%
  dplyr::mutate(
    Last.Trade = as.Date(Last.Trade, "%Y-%m-%d", tz = "UTC"),
    First.Notice = as.Date(First.Notice, "%Y-%m-%d", tz = "UTC"),
    First.Delivery = as.Date(First.Delivery, "%Y-%m-%d", tz = "UTC"),
    Last.Delivery = as.Date(Last.Delivery, "%Y-%m-%d", tz = "UTC")
  )


LGO <- read_csv("https://www.theice.com/api/productguide/spec/34361119/expiry/csv",
  col_types = cols(
    `CONTRACT SYMBOL` = col_skip(),
    FTD = col_skip(), LTD = col_date(format = "%m/%d/%Y"),
    FND = col_date(format = "%m/%d/%Y"),
    LND = col_skip(), FDD = col_date(format = "%m/%d/%Y"),
    LDD = col_date(format = "%m/%d/%Y"),
    FSD = col_skip()
  )
) %>%
  dplyr::transmute(
    cmdty = "icegasoil", tick.prefix = "LGO", Last.Trade = LTD,
    First.Notice = FND, First.Delivery = FDD, Last.Delivery = LDD
  )

LCO <- read_csv("https://www.theice.com/api/productguide/spec/219/expiry/csv",
  col_types = cols(
    `CONTRACT SYMBOL` = col_skip(),
    FTD = col_skip(), LTD = col_date(format = "%m/%d/%Y"),
    FND = col_date(format = "%m/%d/%Y"),
    LND = col_skip(), FDD = col_date(format = "%m/%d/%Y"),
    LDD = col_date(format = "%m/%d/%Y"),
    FSD = col_skip()
  )
) %>%
  dplyr::transmute(
    cmdty = "icebrent", tick.prefix = "LCO", Last.Trade = LTD,
    First.Notice = FND,
    First.Delivery = lubridate::rollback(First.Notice, roll_to_first = TRUE) + months(2),
    Last.Delivery = lubridate::rollback(First.Delivery + months(1))
  )

TMW <- read_csv("https://www.theice.com/api/productguide/spec/27066814/expiry/csv",
                col_types = cols(
                  `CONTRACT SYMBOL` = col_skip(),
                  FTD = col_skip(), LTD = col_date(format = "%m/%d/%Y"),
                  FND = col_date(format = "%m/%d/%Y"),
                  LND = col_skip(), FDD = col_date(format = "%m/%d/%Y"),
                  LDD = col_date(format = "%m/%d/%Y"),
                  FSD = col_skip()
                )
) %>%
  dplyr::transmute(
    cmdty = "icecancrude", tick.prefix = "TMW", Last.Trade = LTD,
    First.Notice = FND,
    First.Delivery = lubridate::rollback(First.Notice, roll_to_first = TRUE) + months(1),
    Last.Delivery = lubridate::rollback(First.Delivery + months(1))
  )

TMR <- read_csv("https://www.theice.com/api/productguide/spec/31687075/expiry/csv",
                col_types = cols(
                  `CONTRACT SYMBOL` = col_skip(),
                  FTD = col_skip(), LTD = col_date(format = "%m/%d/%Y"),
                  FND = col_date(format = "%m/%d/%Y"),
                  LND = col_skip(), FDD = col_date(format = "%m/%d/%Y"),
                  LDD = col_date(format = "%m/%d/%Y"),
                  FSD = col_skip()
                )
) %>%
  dplyr::transmute(
    cmdty = "icecancrude", tick.prefix = "TMR", Last.Trade = LTD,
    First.Notice = FND,
    First.Delivery = lubridate::rollback(First.Notice, roll_to_first = TRUE) + months(1),
    Last.Delivery = lubridate::rollback(First.Delivery + months(1))
  )

TMS <- read_csv("https://www.theice.com/api/productguide/spec/27066815/expiry/csv",
                col_types = cols(
                  `CONTRACT SYMBOL` = col_skip(),
                  FTD = col_skip(), LTD = col_date(format = "%m/%d/%Y"),
                  FND = col_date(format = "%m/%d/%Y"),
                  LND = col_skip(), FDD = col_date(format = "%m/%d/%Y"),
                  LDD = col_date(format = "%m/%d/%Y"),
                  FSD = col_skip(), `OPTIONS FTD` = col_skip(),
                  `OPTIONS LTD` = col_skip()
                )
) %>%
  dplyr::transmute(
    cmdty = "icecancrude", tick.prefix = "TMS", Last.Trade = LTD,
    First.Notice = FND,
    First.Delivery = lubridate::rollback(First.Notice, roll_to_first = TRUE) + months(1),
    Last.Delivery = lubridate::rollback(First.Delivery + months(1))
  )

TMF <- read_csv("https://www.theice.com/api/productguide/spec/27066813/expiry/csv",
                col_types = cols(
                  `CONTRACT SYMBOL` = col_skip(),
                  FTD = col_skip(), LTD = col_date(format = "%m/%d/%Y"),
                  FND = col_date(format = "%m/%d/%Y"),
                  LND = col_skip(), FDD = col_date(format = "%m/%d/%Y"),
                  LDD = col_date(format = "%m/%d/%Y"),
                  FSD = col_skip()
                )
) %>%
  dplyr::transmute(
    cmdty = "icecancrude", tick.prefix = "TMF", Last.Trade = LTD,
    First.Notice = FND,
    First.Delivery = lubridate::rollback(First.Notice, roll_to_first = TRUE) + months(1),
    Last.Delivery = lubridate::rollback(First.Delivery + months(1))
  )

ARV <- read_csv("https://www.ice.com/api/productguide/spec/67689141/expiry/csv",
                col_types = cols(
                  `CONTRACT SYMBOL` = col_skip(),
                  FTD = col_skip(), LTD = col_date(format = "%m/%d/%Y"),
                  FND = col_date(format = "%m/%d/%Y"),
                  LND = col_skip(), FDD = col_date(format = "%m/%d/%Y"),
                  LDD = col_date(format = "%m/%d/%Y"),
                  FSD = col_skip(), `OPTIONS FTD` = col_skip(),
                  `OPTIONS LTD` = col_skip()
                )
) %>%
  dplyr::transmute(
    cmdty = "icecancrude", tick.prefix = "ARV", Last.Trade = LTD,
    First.Notice = FND,
    First.Delivery = lubridate::rollback(First.Notice, roll_to_first = TRUE) + months(1),
    Last.Delivery = lubridate::rollback(First.Delivery + months(1))
  )

TI <- read_csv("https://www.theice.com/api/productguide/spec/213/expiry/csv",
                col_types = cols(
                  `CONTRACT SYMBOL` = col_skip(),
                  FTD = col_skip(), LTD = col_date(format = "%m/%d/%Y"),
                  FND = col_date(format = "%m/%d/%Y"),
                  LND = col_skip(), FDD = col_date(format = "%m/%d/%Y"),
                  LDD = col_date(format = "%m/%d/%Y"),
                  FSD = col_skip()
                )
) %>%
  dplyr::transmute(
    cmdty = "icewti", tick.prefix = "T", Last.Trade = LTD,
    First.Notice = FND,
    First.Delivery = lubridate::rollback(First.Notice, roll_to_first = TRUE) + months(1),
    Last.Delivery = lubridate::rollback(First.Delivery + months(1))
  )

# destfile <- "Download.xls"
# curl::curl_download(url = "https://www.cmegroup.com/CmeWS/mvc/ProductCalendar/Download.xls?productId=425",destfile)
CL <- read_excel("CL.xls", col_types = c(
  "skip", "skip", "skip", "text", "skip", "skip",
  "skip", "skip", "skip", "text", "skip", "text", "text"
)) %>%
  dplyr::as_tibble(.name_repair = "universal") %>%
  dplyr::transmute(
    cmdty = "cmewti", tick.prefix = "CL",
    Last.Trade = as.Date(Last.Trade, "%Y-%m-%d", tz = "UTC"),
    First.Notice = as.Date(First.Notice, "%Y-%m-%d", tz = "UTC"),
    First.Delivery = as.Date(First.Delivery, "%Y-%m-%d", tz = "UTC"),
    Last.Delivery = as.Date(Last.Delivery, "%Y-%m-%d", tz = "UTC")
  )

# curl::curl_download(url = "https://www.cmegroup.com/CmeWS/mvc/ProductCalendar/Download.xls?productId=424",destfile)

BZ <- read_excel("BZ.xls", col_types = c(
  "skip", "skip", "skip", "text", "text", "skip",
  "skip", "skip", "skip", "text", "skip", "text", "text"
)) %>%
  dplyr::as_tibble(.name_repair = "universal") %>%
  dplyr::transmute(
    cmdty = "cmebrent", tick.prefix = "BZ",
    Last.Trade = as.Date(Last.Trade, "%Y-%m-%d", tz = "UTC"),
    First.Notice = as.Date(Settlement, "%Y-%m-%d", tz = "UTC"),
    First.Delivery = lubridate::rollback(First.Notice, roll_to_first = TRUE) + months(2),
    Last.Delivery = First.Delivery
  )

# curl::curl_download(url = "https://www.cmegroup.com/CmeWS/mvc/ProductCalendar/Download.xls?productId=426",destfile)
HO <- read_excel("HO.xls", col_types = c(
  "skip", "skip", "skip", "text", "skip", "skip",
  "skip", "skip", "skip", "text", "skip", "text", "text"
)) %>%
  dplyr::as_tibble(.name_repair = "universal") %>%
  dplyr::transmute(
    cmdty = "cmeulsd", tick.prefix = "HO",
    Last.Trade = as.Date(Last.Trade, "%Y-%m-%d", tz = "UTC"),
    First.Notice = as.Date(First.Notice, "%Y-%m-%d", tz = "UTC"),
    First.Delivery = as.Date(First.Delivery, "%Y-%m-%d", tz = "UTC"),
    Last.Delivery = as.Date(Last.Delivery, "%Y-%m-%d", tz = "UTC")
  )


# curl::curl_download(url = "https://www.cmegroup.com/CmeWS/mvc/ProductCalendar/Download.xls?productId=429",destfile)
RB <- read_excel("RB.xls", col_types = c(
  "skip", "skip", "skip", "text", "skip", "skip",
  "skip", "skip", "skip", "text", "skip", "text", "text"
)) %>%
  dplyr::as_tibble(.name_repair = "universal") %>%
  dplyr::transmute(
    cmdty = "cmerbob", tick.prefix = "RB",
    Last.Trade = as.Date(Last.Trade, "%Y-%m-%d", tz = "UTC"),
    First.Notice = as.Date(First.Notice, "%Y-%m-%d", tz = "UTC"),
    First.Delivery = as.Date(First.Delivery, "%Y-%m-%d", tz = "UTC"),
    Last.Delivery = as.Date(Last.Delivery, "%Y-%m-%d", tz = "UTC")
  )

GC <- read_excel("GC.xls", col_types = c(
  "skip", "skip", "skip", "text", "skip", "skip",
  "skip", "skip", "skip", "text", "skip", "text", "text"
)) %>%
  dplyr::as_tibble(.name_repair = "universal") %>%
  dplyr::transmute(
    cmdty = "comexgold", tick.prefix = "GC",
    Last.Trade = as.Date(Last.Trade, "%Y-%m-%d", tz = "UTC"),
    First.Notice = as.Date(First.Notice, "%Y-%m-%d", tz = "UTC"),
    First.Delivery = as.Date(First.Delivery, "%Y-%m-%d", tz = "UTC"),
    Last.Delivery = as.Date(Last.Delivery, "%Y-%m-%d", tz = "UTC")
  )

SI <- read_excel("SI.xls", col_types = c(
  "skip", "skip", "skip", "text", "skip", "skip",
  "skip", "skip", "skip", "text", "skip", "text", "text"
)) %>%
  dplyr::as_tibble(.name_repair = "universal") %>%
  dplyr::transmute(
    cmdty = "comexsilver", tick.prefix = "SI",
    Last.Trade = as.Date(Last.Trade, "%Y-%m-%d", tz = "UTC"),
    First.Notice = as.Date(First.Notice, "%Y-%m-%d", tz = "UTC"),
    First.Delivery = as.Date(First.Delivery, "%Y-%m-%d", tz = "UTC"),
    Last.Delivery = as.Date(Last.Delivery, "%Y-%m-%d", tz = "UTC")
  )

ALI <- read_excel("ALI.xls", col_types = c(
  "skip", "skip", "skip", "text", "skip", "skip",
  "skip", "skip", "skip", "text", "skip", "text", "text"
)) %>%
  dplyr::as_tibble(.name_repair = "universal") %>%
  dplyr::transmute(
    cmdty = "comexalu", tick.prefix = "ALI",
    Last.Trade = as.Date(Last.Trade, "%Y-%m-%d", tz = "UTC"),
    First.Notice = as.Date(First.Notice, "%Y-%m-%d", tz = "UTC"),
    First.Delivery = as.Date(First.Delivery, "%Y-%m-%d", tz = "UTC"),
    Last.Delivery = as.Date(Last.Delivery, "%Y-%m-%d", tz = "UTC")
  )

LTH <- read_excel("LTH.xls") %>%
  dplyr::as_tibble(.name_repair = "universal") %>%
  dplyr::transmute(
    cmdty = "comexlithium", tick.prefix = "LTH",
    Last.Trade = as.Date(Last.Trade, "%Y-%m-%d", tz = "UTC"),
    First.Notice = Last.Trade,
    First.Delivery = Last.Trade,
    Last.Delivery = Last.Trade
  )

HG <- read_excel("HG.xls", col_types = c(
  "skip", "skip", "skip", "text", "skip", "skip",
  "skip", "skip", "skip", "text", "skip", "text", "text"
)) %>%
  dplyr::as_tibble(.name_repair = "universal") %>%
  dplyr::transmute(
    cmdty = "comexcopper", tick.prefix = "HG",
    Last.Trade = as.Date(Last.Trade, "%Y-%m-%d", tz = "UTC"),
    First.Notice = as.Date(First.Notice, "%Y-%m-%d", tz = "UTC"),
    First.Delivery = as.Date(First.Delivery, "%Y-%m-%d", tz = "UTC"),
    Last.Delivery = as.Date(Last.Delivery, "%Y-%m-%d", tz = "UTC")
  )

bbdate <- function(x){
  tmp <- as.numeric(substring(x,7,8))
  tmp <- ifelse(tmp >= 60, tmp + 1900, tmp + 2000)
  paste0(substr(x,1,6),as.character(tmp))
}

W <- read_excel("W.xls", skip = 4) %>%
  dplyr::as_tibble(.name_repair = "universal") %>%
  dplyr::transmute(cmdty = "cmewheat",
                   tick.prefix = "W",
                   Last.Trade = as.Date(Last.Trade, "%Y-%m-%d", tz = "UTC"),
                   First.Notice = as.Date(First.Notice, "%Y-%m-%d", tz = "UTC"),
                   First.Delivery  = as.Date(First.Holding, "%Y-%m-%d", tz = "UTC"),
                   Last.Delivery  = as.Date(Last.Holding, "%Y-%m-%d", tz = "UTC")
                   )

C <- read_excel("C.xls") %>%
  dplyr::as_tibble(.name_repair = "universal") %>%
  dplyr::transmute(cmdty = "cmecorn",
                   tick.prefix = "C",
                   Last.Trade = as.Date(Last.Trade, "%Y-%m-%d", tz = "UTC"),
                   First.Notice = as.Date(First.Notice, "%Y-%m-%d", tz = "UTC"),
                   First.Delivery  = as.Date(First.Holding, "%Y-%m-%d", tz = "UTC"),
                   Last.Delivery  = as.Date(Last.Holding, "%Y-%m-%d", tz = "UTC")
  )


S <- read_excel("S.xls") %>%
  dplyr::as_tibble(.name_repair = "universal") %>%
  dplyr::transmute(cmdty = "cmesoybean",
                   tick.prefix = "S",
                   Last.Trade = as.Date(Last.Trade, "%Y-%m-%d", tz = "UTC"),
                   First.Notice = as.Date(First.Notice, "%Y-%m-%d", tz = "UTC"),
                   First.Delivery  = as.Date(First.Holding, "%Y-%m-%d", tz = "UTC"),
                   Last.Delivery  = as.Date(Last.Holding, "%Y-%m-%d", tz = "UTC")
  )

## tradeCycle
tradeCycle <- read.csv("tradeCycle.csv", sep = ",", header = TRUE, na.strings = "NA", stringsAsFactors = FALSE) %>%
  dplyr::transmute(
    market = market,
    flowmonth = as.Date(flowmonth),
    trade.cycle.start = lubridate::rollback(dates = as.Date(trade.cycle.end), roll_to_first = TRUE),
    trade.cycle.end = as.Date(trade.cycle.end)
  )
nymex <- holidaysOil %>%
  dplyr::filter(key == "nymex") %>%
  dplyr::select(value) %>%
  .[[1]]

bizdays::create.calendar(name = "nymex", holidays = nymex, weekdays = c("saturday", "sunday"))
tradeCycle <- expiry_table %>%
  dplyr::filter(cmdty == "cmewti") %>%
  dplyr::mutate(trade.cycle.start = bizdays::offset(dplyr::lag(Last.Trade), 4, "nymex")) %>%
  tidyr::drop_na() %>%
  #dplyr::select(Last.Trade) %>%
  dplyr::transmute(
    market = "usdomestic",
    flowmonth = lubridate::rollback(dates = Last.Trade + months(1), roll_to_first = TRUE),
    trade.cycle.start = trade.cycle.start,
    trade.cycle.end = bizdays::offset(Last.Trade, 3, "nymex")
  ) %>%
  tidyr::drop_na() %>%
  rbind(tradeCycle, .) %>%
  dplyr::as_tibble() %>%
  dplyr::mutate(trade.cycle.end = dplyr::case_when(trade.cycle.end == as.Date("2023-11-24") ~ as.Date("2023-11-22"), # us domestic Argus holiday
                                                   TRUE ~ trade.cycle.end))

bizDays <- function(from = as.Date("2023-02-01"), to = as.Date("2023-02-13"), calendar = "nymex", output = "tradingDays") {
  calDays <- seq(from = as.Date(from), to = as.Date(to), by = "day")
  hol <- RTL::holidaysOil %>% dplyr::filter(key == "calendar")
  out <- list()
  dates <- calDays[!(calDays %in% hol$value)]
  out$dates <- dates[!(base::weekdays(dates) %in% c("Saturday", "Sunday"))]
  out$tradingDays <- length(out$dates)
  if (output == "tradingDays") {return(out$tradingDays)} else {return(out$dates)}
}

tradeCycle <- tradeCycle %>%
  dplyr::mutate(
    daysInCycle = purrr::pmap(.l = list(from = trade.cycle.start,
                                        to = trade.cycle.end,
                                        calendar = "nymex"),
                                        .f=bizDays),
    daysInCycle = as.numeric(daysInCycle),
    dailyWt = 1/ daysInCycle)

tcycle <- function(trade.cycle.start = as.Date("2016-11-01"),
                trade.cycle.end = as.Date("2016-11-17")) {
  # Pricing days
  calDays <- seq(from = trade.cycle.start, to = trade.cycle.end, by = "day")
  hol <- RTL::holidaysOil %>% dplyr::filter(key == "nymex")
  bizDays <- calDays[!(calDays %in% hol$value)]
  bizDays <- bizDays[!(weekdays(bizDays) %in% c("Saturday", "Sunday"))]
  pricedIn <- cumsum(base::rep(x = 1/length(bizDays),length(bizDays)))
  out <- list()
  out[["bizDays"]] <- bizDays
  out[["pricedIn"]] <- pricedIn
  return(out)
  }

tradeCycle <- tradeCycle %>%
  dplyr::mutate(x = purrr::pmap(.l = list(trade.cycle.start, trade.cycle.end), .f = tcycle)) %>%
  tidyr::unnest_wider(col = x)

usethis::use_data(tradeCycle, overwrite = TRUE)

# CME WTi MEH
meh <- RTL::tradeCycle %>%
  dplyr::filter(market == "usdomestic",
                flowmonth > "2018-01-01") %>%
  dplyr::transmute(cmdty = "cmewtihou",
                   tick.prefix = "HTT",
                   Last.Trade = trade.cycle.end,
                   First.Notice = Last.Trade,
                   First.Delivery = lubridate::rollback(Last.Trade, roll_to_first = TRUE) + months(1),
                   Last.Delivery = First.Delivery + months(1) - 1)


expiry_table <- rbind(expiry_table, LCO, LGO, CL, BZ, HO, RB, GC, SI, ALI, LTH, HG, W, C ,S,meh,ARV,TI,TMW,TMF,TMR,TMS) %>%
  dplyr::distinct() %>%
  dplyr::mutate(
    Year = year(First.Delivery),
    Month = month(First.Delivery),
    Month.Letter = futmonths[Month]
  ) #%>% dplyr::filter(Year > 2003)

usethis::use_data(expiry_table, overwrite = T)


# cma ---------------------------------------------------------------------

swp <- function(Month = "2023-01-01",
                          contract = "cmewti",
                          exchange = "nymex") {
  # Pricing days
  m <- as.Date(Month)
  m1 <- lubridate::rollback(m + months(1))
  calDays <- seq(as.Date(m), m1, by = "day")
  hol <- RTL::holidaysOil %>% dplyr::filter(key == exchange)
  bizDays <- calDays[!(calDays %in% hol$value)]
  bizDays <- bizDays[!(weekdays(bizDays) %in% c("Saturday", "Sunday"))]
  pricedIn <- cumsum(base::rep(x = 1/length(bizDays),length(bizDays)))

  # Expiries
  expiry <- RTL::expiry_table %>%
    dplyr::filter(
      cmdty == contract,
      Last.Trade >= m,
      Last.Trade <= m1
    ) %>%
    dplyr::pull(Last.Trade)

  x <- dplyr::tibble(date = bizDays, Up2expiry = ifelse(date <= expiry, 1, 0))
  numDaysFut1 <- sum(x$Up2expiry)
  numDaysFut2 <- nrow(x) - sum(x$Up2expiry)
  first.fut.weight <- sum(x$Up2expiry) / nrow(x)

  out <- list()
  out[["bizDays"]] <- bizDays
  out[["pricedIn"]] <- pricedIn
  out[["expiry"]] <- expiry
  out[["numDaysFut1"]] <- numDaysFut1
  out[["numDaysFut2"]] <- numDaysFut2
  out[["percentFut1"]] <- first.fut.weight
  return(out)
}


cma <-  dplyr::tibble(swapMonth = seq.Date(from = as.Date("2015-01-01"), to = as.Date("2030-12-01"), by = "month")) %>%
  dplyr::mutate(swp = purrr::pmap(.l = list(Month = swapMonth), .f = swp)) %>%
  tidyr::unnest_wider(col = swp)

usethis::use_data(cma, overwrite = T)



# crudeOil dataset

crudeOil <- list()

  ## Canadian Crude Data

url = "https://crudemonitor.ca/api/json.php?condensates%5B0%5D=Cochin+Condensate&condensates%5B1%5D=Condensate+Blend&condensates%5B2%5D=Fort+Saskatchewan+Condensate&condensates%5B3%5D=Peace+Condensate&condensates%5B4%5D=Pembina+Condensate&condensates%5B5%5D=Rangeland+Condensate&condensates%5B6%5D=Southern+Lights+Diluent&crudes%5B0%5D=Federated&crudes%5B1%5D=Light+Smiley&crudes%5B2%5D=Peace&crudes%5B3%5D=Pembina&crudes%5B4%5D=Secure+Sask+Light&crudes%5B5%5D=Mixed+Sweet+Blend&crudes%5B6%5D=Rainbow&crudes%5B7%5D=Koch+Alberta&crudes%5B8%5D=Midale&crudes%5B9%5D=CNRL+Light+Sweet+Synthetic&crudes%5B10%5D=Husky+Synthetic+Blend&crudes%5B11%5D=Long+Lake+Light+Synthetic&crudes%5B12%5D=Premium+Albian+Synthetic&crudes%5B13%5D=Shell+Synthetic+Light&crudes%5B14%5D=Suncor+Synthetic+A&crudes%5B15%5D=Syncrude+Sweet+Premium&crudes%5B16%5D=Bow+River+North&crudes%5B17%5D=Bow+River+South&crudes%5B18%5D=Fosterton&crudes%5B19%5D=Lloyd+Blend&crudes%5B20%5D=Seal+Heavy&crudes%5B21%5D=Smiley-Coleville&crudes%5B22%5D=Wabasca+Heavy&crudes%5B23%5D=Western+Canadian+Blend&crudes%5B24%5D=Access+Western+Blend&crudes%5B25%5D=Canadian+Natural+High+TAN&crudes%5B26%5D=Christina+Dilbit+Blend&crudes%5B27%5D=Cold+Lake&crudes%5B28%5D=Fort+Hills+Dilbit&crudes%5B29%5D=Kearl+Lake&crudes%5B30%5D=Surmont+Heavy+Dilbit&crudes%5B31%5D=Western+Canada+Dilbit&crudes%5B32%5D=Western+Canadian+Select&crudes%5B33%5D=Albian+Heavy+Synthetic&condensateProperties%5B0%5D=condensates-BA&crudeProperties%5B0%5D=crudes-BA&date%5Bstart%5D=&date%5Bend%5D=2023-09-29"

samples <- httr::GET(url) %>% httr::content(.,as="text") %>% jsonlite::fromJSON(.) %>% dplyr::as_tibble()

names(samples) <- c("Name", "Batch","date","Location","Density","Gravity","Sulphur","MCR","Viscosity", "Sediment","RVP","Olefins","OrganoPhosphorus","Oxygenates","TAN","Salt","Nickel","Vanadium")
CanadaAssays <- samples %>%
  dplyr::mutate(date = as.Date(date),
                Grade = gsub("-.*$","",Batch),
                Location = case_when(grepl("AHS|AWB|CAL|KDB|MSW|PSO|WDB|WH|KDB|CL\\(E\\)|SHB|P|SSP|SW|MPR|FD", Grade) ~ "Edmonton",
                                     grepl("CHN|CRW|CFT|CPR|CPM|CRL|SLD", Grade) ~ "Edmonton",
                                     grepl("BRN|CDB|LLB|LLK|WCS|CL|CL\\(H\\)", Grade) ~ "Hardisty",
                                     grepl("MSM|LSB", Grade) ~ "Cromer",
                                     grepl("MSY|MSE", Grade) ~ "Kerrobert",
                                     TRUE ~ "unknown"),
                Grade = case_when(grepl("CL", Grade) ~ "CL", TRUE ~ Grade)
                ) %>%
  #dplyr::select(-Name) %>%
  dplyr::select(Name, Grade,Location, date,Batch, everything()) %>%
  dplyr::group_by(Name,Grade,Location,date) %>%
  dplyr::mutate(date = tsibble::yearmonth(date)) %>%
  dplyr::mutate(across(Density:Vanadium,as.numeric)) %>%
  dplyr::summarise_if(is.numeric, mean, na.rm = TRUE)

crudeOil$CanadianAssays <- CanadaAssays %>% dplyr::select(-Name)

crudeOil$CanadaPrices <- readRDS("crude_prices.RDS") %>%
  dplyr::transmute(Ticker = Ticker, date = tsibble::yearmonth(YM), Value = Value)

  ## BP Assays
    ### capline https://cappl.com/Reports1.aspx
library(rvest)
url <- "https://www.bp.com/en/global/bp-trading-and-shipping/documents-and-downloads/technical-downloads/crudes-assays.html"
html <- xml2::read_html(url)

  ## Simplified tables
x <- html %>%
  rvest::html_nodes("table") %>%
  rvest::html_table(fill = T) %>%
  .[[1]] %>%
  dplyr::as_tibble() %>%
  dplyr::slice(-1) %>%
  dplyr::select(1:5) %>%
  dplyr::transmute(Crude = X1, Country = X2, API = as.numeric(X3), Sulphur = as.numeric(X4), TAN = as.numeric(X5))

y <- CanadaAssays %>%
  dplyr::filter(date == dplyr::last(date)) %>%
  dplyr::ungroup() %>%
  dplyr::transmute(Name,
                   Country = "Canada",API = Gravity, Sulphur, TAN
  ) %>%
  dplyr::ungroup() %>%
  dplyr::rename(Crude = Name)

crudes <- rbind(x, y) %>%
  tibble::add_row(Crude = "Tapis", Country = "Malaysia", API = 42.7, Sulphur = .044, TAN = 0.215) %>%
  tibble::add_row(Crude = "Maya", Country = "Mexico", API = 22, Sulphur = 3.3, TAN = 0.3) %>%
  tibble::add_row(Crude = "Light Louisiana Sweet", Country = "US", API = 38.5, Sulphur = .40, TAN = 0.25) %>%
  tibble::add_row(Crude = "West Texas Intermediate", Country = "US", API = 40.6, Sulphur = .22, TAN = 0.1) %>%
  tibble::add_row(Crude = "Napo", Country = "Ecuador", API = 17, Sulphur = 2.30, TAN = 0.5) %>%
  tibble::add_row(Crude = "Oriente", Country = "Ecuador", API = 23, Sulphur = 1.60, TAN = 0.25) %>%
  tibble::add_row(Crude = "Urals", Country = "Russia", API = 31.75, Sulphur = 1.35, TAN = 0.05)


crudes <- crudes %>%
  dplyr::mutate(
    SweetSour = case_when(Sulphur < 0.5 ~ "Sweet", TRUE ~ "Sour"),
    LightMedHeavy = case_when(
      API < 22.3 ~ "Heavy",
      API > 31.1 ~ "Light",
      TRUE ~ "Medium"
    ),
    Benchmark = if_else(Crude %in% c(
      "Maya", # "Oriente","Napo",
      "West Texas Intermediate", "Light Louisiana Sweet",
      "Oman Export Blend", "Mars",
      "Brent", "Tapis"
    ), "yes", "no"),
    Notes = case_when(
      Country == "Canada" ~ "Canada",
      Benchmark == "yes" ~ "Benchmark",
      TRUE ~ "Others"
    ),
  )

crudes$LightMedHeavy <- factor(crudes$LightMedHeavy, levels = c("Light", "Medium", "Heavy"))
crudes$SweetSour <- factor(crudes$SweetSour, levels = c("Sweet", "Sour"))
crudeOil$crudes <- crudes %>% dplyr::arrange(Crude) %>% dplyr::slice(-1)

## xls assays
css <- "body > div.aem-Grid.aem-Grid--12.aem-Grid--default--12 > div:nth-child(3) > div > div > div.nr-table-component.nr-component.aem-GridColumn.aem-GridColumn--default--12"
urls <- html %>%
  html_nodes(css = css) %>%
  html_nodes("a") %>%
  rvest::html_attr("href") %>%
  as_tibble() %>%
  dplyr::transmute(xls = paste0("https://www.bp.com", value)) %>%
  unique() %>%
  dplyr::mutate(cn = str_replace_all(
    xls,
    c("https://www.bp.com/content/dam/bp/business-sites/en/global/bp-trading-and-shipping/documents/technical-documents-and-downloads/crudes/" = "", ".xls" = "")
  ))


crudeassaysBP <- list()
for (i in 1:nrow(urls)) {
  destfile <- urls$cn[i]
  curl::curl_download(
    url = urls$xls[i],
    destfile = destfile
  )
  tmp <- readxl::read_excel(destfile, range = "B30:P85", col_names = F)
  colnames(tmp) <- c("Specification", "Whole.crude", "Light.Naphtha", "Heavy.Naphtha1", "Heavy.Naphtha2", "Kero", "Light.Gas.Oil", "HeavyGasOil", "Light.VGO", "Heavy.VGO1", "Heavy.VGO2", "AtRes1", "AtRes2", "VacRes1", "VacRes2")
  tmp <- tmp %>%
    tidyr::drop_na("Specification") %>%
    #dplyr::na_if("-") %>%
    dplyr::mutate_at(vars(!starts_with("Spec")), as.numeric) %>%
    as_tibble() %>%
    dplyr::mutate(Specification = stringr::str_replace_all(Specification,c("°" = "", "%" = "perc")))
  crudeassaysBP[[destfile]] <- tmp
  file.remove(destfile)
}

crudeOil$bpAssays <- crudeassaysBP

  # Exxon Assays

url <- "https://corporate.exxonmobil.com/Crude-oils/Crude-trading/Crude-oil-blends-by-API-gravity-and-by-sulfur-content#APIgravity"
html <- xml2::read_html(url)
css <- "span.p:nth-child(1)"

urls <- html %>%
  html_nodes(css = css) %>%
  html_nodes("a") %>%
  rvest::html_attr("href") %>%
  as_tibble() %>%
  dplyr::filter(grepl("/energy-supply/crude-trading/", value)) %>%
  dplyr::transmute(site = paste0("https://corporate.exxonmobil.com", value)) %>%
  unique()

fetchURL <- function(url, css, prefix) {
  xml2::read_html(url) %>%
    html_nodes(css = css) %>%
    html_nodes("a") %>%
    rvest::html_attr("href") %>%
    paste0(prefix, .)
}

prefix = "https://corporate.exxonmobil.com"
css = "body > main > div.article--wrapper > section.articleMedia.articleMedia-in-line.articleMedia--relatedContent > div > article > div > div:nth-child(3) > h3"
xls <- ""
for (i in 1:length(urls$site)) {
  xls[i] <- xml2::read_html(urls$site[i]) %>%
    html_nodes(css = css) %>%
    html_nodes("a") %>%
    rvest::html_attr("href") %>%
    paste0(prefix, .)
}

urls$xls <- xls

urls <- urls %>%
  dplyr::filter(!grepl(".pdf", xls, ignore.case = T)) %>%
  dplyr::mutate(cn = gsub("https://corporate.exxonmobil.com/what-we-do/energy-supply/crude-trading/","",site))

crudeassaysXOM <- list()
for (i in 1:nrow(urls)) {
  destfile <- paste0(urls$cn[i],".xlsx")
  curl::curl_download(
    url = urls$xls[i],
    destfile = destfile
  )
  tmp <- readxl::read_excel(destfile, skip = 4) %>%
    dplyr::rename_all(list(~ make.names(.)))
  #tmp[,1] <- gsub("°","",tmp[,1])
  #tmp[,1] <- gsub("%","perc",tmp[,1])
  colnames(tmp)[1] <- gsub(".xlsx","",destfile)
  crudeassaysXOM[[gsub(".xlsx","",destfile)]] <- tmp
  file.remove(destfile)
}

crudeOil$xomAssays <- crudeassaysXOM

rm(html, tmp, urls, css, destfile, i)

usethis::use_data(crudeOil, overwrite = T)

## Physical Diffs
fizdiffs <- readRDS("fizdiffs.RDS")
usethis::use_data(fizdiffs, overwrite = T)

## Trading Hubs

tradeHubs <-
  dplyr::tibble(
    lat = c(53.54608, 52.64079, 35.94033, 29.99062, 29.73144, 49.28829),
    long = c(-113.34786, -111.27061, -96.74633, -94.00553,-95.12680, -122.95403),
    hub = c("Edmonton", "Hardisty", "Cushing", "Nederland","Mont Belvieu", "Burnaby")
  )
usethis::use_data(tradeHubs, overwrite = T)


## FX forwards

fxfwd <- list()

fromDate <- Sys.Date() - lubridate::years(2)
fxfwd$historical <-
  RTL::getPrices(
    feed = "Morningstar_FX_Forwards",
    contracts = c("USDCAD 1Y", "USDCAD 5Y"),
    from = fromDate,
    iuser = mstar[[1]],
    ipassword = mstar[[2]]
  )

rD <- rsDriver(browser = "firefox", chromever = NULL)
remDr <- rD[["client"]]
Sys.sleep(2)
remDr$navigate("https://ca.investing.com/rates-bonds/forward-rates")
Sys.sleep(2)
elem <- remDr$findElement(using = 'class', value = 'selectBox')
elem$findChildElements("css","option")[[7]]$clickElement()
page <- remDr$getPageSource()
remDr$close()
fxfwd$curve <- read_html(page[[1]]) %>% rvest::html_table(fill = TRUE) %>% .[[3]] %>%
  dplyr::select(maturity = Name, bid = Bid,ask = Ask) %>%
  dplyr::filter(!grepl("TN|SN",maturity)) %>%
  dplyr::mutate(maturity = gsub("USDCAD|FWD","",maturity),
                maturity = gsub("SW","1W",maturity),
                mid = (bid + ask)/2) %>%
  dplyr::mutate(term = dplyr::case_when(grepl("ON",maturity) ~ 1/365,
                                        grepl("M",maturity) ~ readr::parse_number(maturity)/12,
                                        grepl("Y",maturity) ~ readr::parse_number(maturity),
                                        grepl("W",maturity) ~ readr::parse_number(maturity)/52,
                                        TRUE ~ 0
  ))

usethis::use_data(fxfwd, overwrite = T)

# Refinery Optimization

refineryLPdata <- list()

refineryLPdata$inputs <- data.frame(
  info = c("price", "processing.fee"),
  LightSweet = c(-50, -1),
  HeavySour = c(-30, -4)
)

refineryLPdata$outputs <- data.frame(
  product = c("mogas", "distillate", "fo", "resid"),
  prices = c(70, 70, 30, 20),
  max.prod = c(50000, 40000, 20000, 10000),
  LightSweet.yield = c(0.65, .20, .10, 0.05),
  HeavySour.yield = c(0.40, 0.20, .30, .1)
)

usethis::use_data(refineryLPdata, overwrite = T)


# Educational Dataset
tradeprocess <- RTL::getPrices(
  feed = "CME_NymexFutures_EOD", contracts = c("@CL24H", "@HO24F", "@HO24H", "@LT24H"),
  from = "2021-01-01", iuser = mstar[[1]], ipassword = mstar[[2]]
) %>% stats::na.omit()
usethis::use_data(tradeprocess, overwrite = T)



# IR  -------------------------------------------------

  ## Orbital
url <- "https://nssdc.gsfc.nasa.gov/planetary/factsheet/index.html"
html <- xml2::read_html(url)

planets <- html %>%
  rvest::html_nodes("table") %>%
  rvest::html_table(header = TRUE) %>%
  .[[1]]
colnames(planets)[1] <- "Metric"
planets <- planets %>%
  dplyr::as_tibble() %>%
  dplyr::rename_all(.funs = stringr::str_to_title) %>%
  dplyr::mutate_all(function(x) gsub(",|\\*", "", x)) %>%
  dplyr::mutate(dplyr::across(.cols = -Metric, .fns = as.numeric)) %>%
  na.omit() %>%
  tidyr::pivot_longer(-Metric, names_to = "Planet", values_to = "value") %>%
  tidyr::pivot_wider(names_from = Metric, values_from = value)
usethis::use_data(planets, overwrite = T)

# Curves and Def - barchart
library(RSelenium)
library(rvest)
library(tidyverse)
rD <- rsDriver(port = 4545L, browser = "firefox", chromever = NULL)
#rD <- rsDriver(port = 4444L, browser = "chrome",chromever = "latest", verbose = FALSE)
remDr <- rD[["client"]]
Sys.sleep(2)

remDr$navigate("https://www.chathamfinancial.com/technology/us-market-rates")
Sys.sleep(2)
remDr$findElement(using = 'css', value = 'div.rates:nth-child(8) > div:nth-child(1) > div:nth-child(2) > table:nth-child(1)')$clickElement()  # Cross rates
#remDr$findElement(using = 'class', value = 'bc-table-wrapper')$clickElement()
page <- remDr$getPageSource()
chat <-  rvest::read_html(page[[1]]) %>%
  rvest::html_table()
libor <- chat %>% .[[4]] %>%
  dplyr::rename(Name = 1, Last = 2) %>%
  dplyr::select(1,2) %>%
  dplyr::filter(Name %in% c("SOFR","1-month Term SOFR","3-month Term SOFR")) %>%
  dplyr::mutate(Name = c("d1w", "d1m", "d3m"),
                Last = readr::parse_number(Last)/100)
#libor <- rbind(dplyr::tibble(Name = "d1w", Last = libor$Last[1]),libor)
irs <- chat %>% .[[3]] %>%
  dplyr::rename(Name = 1, Last = 2) %>%
  dplyr::select(Name,Last) %>%
  dplyr::slice(-1) %>%
  dplyr::mutate(Name = paste0("s",c(2,3,5,7,10,15,30),"y"),
                Last = readr::parse_number(Last)/100)
#remDr$navigate("https://www.cmegroup.com/markets/interest-rates/stirs/eurodollar.settlements.html")
remDr$navigate("https://www.cmegroup.com/markets/interest-rates/stirs/three-month-sofr.settlements.html")

Sys.sleep(3)
page <- remDr$getPageSource()
futs <- rvest::read_html(page[[1]]) %>%
  rvest::html_table() %>%
  .[[1]] %>%
  dplyr::slice(-1) %>%
  dplyr::slice(1:8) %>%
  dplyr::transmute(Name = paste0("fut", 1:8), Last = Settle)

remDr$close()
rD[["server"]]$stop()

# Discount Objects

library(RQuantLib)
# removing d1y fro LIBOR  and s2y - causes negative rates
tsQuotes <- rbind(libor, irs, futs) %>% as_tibble() %>%
  dplyr::mutate(Last = readr::parse_number(Last)) %>%
  tidyr::pivot_wider(names_from = Name, values_from = Last) %>%
  dplyr::select(-s2y,-d3m) %>%
  transpose() %>% unlist() %>% as.list()
tradeDate <- as.Date(Sys.Date() - 1)
params <- list(tradeDate = tradeDate, settleDate = tradeDate + 2, dt = 1/12,
               interpWhat = "discount", interpHow = "spline")
setEvaluationDate(tradeDate)
times <- seq(0, 30 - 1/12, 1 / 12)
savepar <- par(mfrow = c(3, 3), mar = c(4, 4, 2, 0.5))
on.exit(par(savepar))
usSwapCurves <- DiscountCurve(params, tsQuotes, times)
# check your curve for negative forward rates
usSwapCurves[1:4] %>%
  dplyr::as_tibble() %>%
  dplyr::select(-discounts) %>%
  tidyr::pivot_longer(-times, names_to = "ir", values_to = "rate") %>%
  plotly::plot_ly(
    x = ~ times,
    y = ~ rate,
    name = ~ ir,
    type = "scatter",
    mode = "lines"
  )
#usSwapCurves[1:4] %>% dplyr::as_tibble() %>% View()
cbind(usSwapCurves$times, usSwapCurves$discounts, usSwapCurves$zerorates, usSwapCurves$forwards) %>%
  dplyr::as_tibble() %>% dplyr::rename(times = V1, discounts = V2, zerorates = V3, forwards = V4) %>%
  arrow::write_feather(x = ., sink = "C:/Users/cotep/data/usd-ir.feather")
usethis::use_data(tsQuotes, overwrite = T)

tsQuotes <- list(flat = 0.04)
usSwapCurvesPar <- DiscountCurve(params, tsQuotes, times)

usethis::use_data(usSwapCurves, overwrite = T)
usethis::use_data(usSwapCurvesPar, overwrite = T)
