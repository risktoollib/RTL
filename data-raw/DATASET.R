# usethis::use_readme_md()
# usethis::use_package("sp")
# usethis::use_pipe()
library(RTL)
library(tidyverse)
library(lubridate)
library(jsonlite)
source("~/keys.R")
setwd("~/RTL/data-raw")

## Sample energy futures datasets
df_fut <- readRDS("df_fut") ; usethis::use_data(df_fut, overwrite = T)

iuser = mstar[["iuser"]] ; ipassword = mstar[["ipassword"]]
startdate <- "2013-01-01"
crude <- c(paste0("CL_",sprintf('%0.3d', 1:36),"_Month"), paste0("NG_",sprintf('%0.3d', 1:36),"_Month"))
crudeICE <- c(paste0("BRN_",sprintf('%0.3d', 1:36),"_Month"))
pdts <- c(paste0("HO_",sprintf('%0.3d', 1:18),"_Month"), paste0("RB_",sprintf('%0.3d', 1:18),"_Month"))
crude <- RTL::getPrices(feed="CME_NymexFutures_EOD_continuous",
               contracts = crude,from = startdate,
               iuser = iuser, ipassword = ipassword) %>%
  pivot_longer(-date,names_to = "series", values_to = "value") %>%
  dplyr::mutate(series = stringr::str_replace_all(series,c("_0" = "","_Month" = ""))) %>% na.omit()
crudeICE <- RTL::getPrices(feed="ICE_EuroFutures_continuous",
                        contracts = crudeICE,from = startdate,
                        iuser = iuser, ipassword = ipassword) %>%
  pivot_longer(-date,names_to = "series", values_to = "value") %>%
  dplyr::mutate(series = stringr::str_replace_all(series,c("_0" = "","_Month" = ""))) %>% na.omit()
pdts <- RTL::getPrices(feed="CME_NymexFutures_EOD_continuous",
                         contracts = pdts,from = startdate,
                         iuser = iuser, ipassword = ipassword) %>%
  pivot_longer(-date,names_to = "series", values_to = "value") %>%
  dplyr::mutate(series = stringr::str_replace_all(series,c("_0" = "","_Month" = ""))) %>% na.omit()

dflong <-  rbind(crude, crudeICE, pdts)
dfwide <- dflong %>% tidyr::pivot_wider(names_from = series, values_from = value) %>% na.omit()
usethis::use_data(dflong, overwrite = T)
usethis::use_data(dfwide, overwrite = T)
rm(crude,crudeICE,pdts)

## Sample EIA dataset

CushingStocks <- RTL::eia2tidy(ticker = "PET.W_EPC0_SAX_YCUOK_MBBL.W", key = EIAkey, name = "CushingStocks")
usethis::use_data(CushingStocks, overwrite = T)

## Sample GIS Mapping
load("map.RData")
crudepipelines <- crudepipes
usethis::use_data(crudepipelines, overwrite = T)
usethis::use_data(refineries, overwrite = T)

ng_storage <- tibble::tribble(~ticker, ~series,"NG.NW2_EPG0_SWO_R48_BCF.W","NG Storage - Lower 48") %>%
  dplyr::mutate(key=EIAkey) %>%
  dplyr::mutate(df = purrr::pmap(list(ticker,key),.f=RTL::eia2tidy)) %>%
  dplyr::select(series, df) %>% tidyr::unnest()
usethis::use_data(ng_storage, overwrite = T)

## EIA Mapping
tickers_eia <- read.csv('eia.csv',sep=",",header=TRUE,na.strings="NA",stringsAsFactors=FALSE)
usethis::use_data(tickers_eia, overwrite = T)

# library(readxl)
# download.file(url="https://www.cmegroup.com/CmeWS/mvc/ProductCalendar/Download.xls?productId=425",
#               destfile = "expiries")
# expiries <- read_excel("expiries")
# colnames(expiries) <- str_replace_all(string=colnames(expiries), pattern=" ", repl="")
# write_csv2(expiries,"~/nymex_wti_expiries.csv")

futmonths = c("F","G","H","J","K","M","N","Q","U","V","X","Z")
expiry_table <- read.csv('expiry_table.csv',sep=",",header=TRUE,na.strings="NA",stringsAsFactors=FALSE)
expiry_table <- dplyr::mutate(expiry_table,
                              Last.Trade = as.Date(as.character(Last.Trade),"%Y-%m-%d",tz="UTC"),
                              First.Notice = as.Date(as.character(First.Notice),"%Y-%m-%d",tz="UTC"),
                              First.Delivery = as.Date(as.character(First.Delivery),"%Y-%m-%d",tz="UTC"),
                              Last.Delivery = as.Date(as.character(Last.Delivery),"%Y-%m-%d",tz="UTC"),
                              Year = year(First.Delivery),
                              Month = month(First.Delivery),
                              Month.Letter = futmonths[Month],
                              ticker = paste(tick.prefix,"_",Year,Month.Letter,sep="")) %>%
  dplyr::filter(Year > 2003)
usethis::use_data(expiry_table, overwrite = T)

## Holiday Calendar
holidaysOil<-read.csv('holidays.csv',sep=",",header=TRUE,na.strings="NA",stringsAsFactors=FALSE) %>%
  dplyr::mutate(nymex = as.Date(as.character(nymex),"%Y-%m-%d",tz="UTC"),
                ice = as.Date(as.character(ice),"%Y-%m-%d",tz="UTC")) %>% tidyr::gather()
holidaysOil <- holidaysOil[complete.cases(holidaysOil),]
usethis::use_data(holidaysOil, overwrite = T)

## tradeCycle
tradeCycle <- read.csv('tradeCycle.csv',sep=",",header=TRUE,na.strings="NA",stringsAsFactors=FALSE)
usethis::use_data(tradeCycle, overwrite = T)

## tweets
  ### Trump
  # http://www.trumptwitterarchive.com/archive
  # use geany text editor in Linux for very large files
twtrump <- fromJSON("twtrump.json")
twtrump <- twtrump %>%
  dplyr::mutate(created_at = as.POSIXct(created_at,tz="GMT",format=c("%a %b %d %H:%M:%S +0000 %Y"))) %>%
  dplyr:::rename(favoriteCount = favorite_count, created = created_at, id = id_str) %>%
  as_tibble()
usethis::use_data(twtrump, overwrite = T)
  ### OOTT
library(twitteR)
setup_twitter_oauth(consumer_key = tw$cons.key, consumer_secret = tw$cons.secret,
                    access_token = tw$access.token, access_secret = tw$access.secret)
twoott <- twitteR::searchTwitter('#OOTT', n = 1e4, since = '2016-11-06', retryOnRateLimit = 1e4)
twoott <- twitteR::twListToDF(twoott) %>% as_tibble()
usethis::use_data(twoott, overwrite = T)

## Canadain Crude Data
cancrudeprices <- readRDS("~/dscf/data/crude_prices.RDS") ; usethis::use_data(cancrudeprices, overwrite = T)
cancrudeassays <- readRDS("~/dscf/data/crude_assays.RDS") ; usethis::use_data(cancrudeassays, overwrite = T)

cancrudeassayssum <- cancrudeassays %>% dplyr::group_by(Ticker,Crude) %>%
  dplyr::filter(YM > "2015-01-01", Ticker != "MSW(S)") %>%
  dplyr::select(-Location,-Sediment,-Salt,-Olefins,-Viscosity) %>%
  dplyr::mutate(TAN = case_when(Ticker == "MSW" ~ 0,TRUE ~ TAN)) %>%
  na.omit() %>% summarise_all(list(mean))
usethis::use_data(cancrudeassayssum, overwrite = T)

library(rvest)
url = "https://www.bp.com/en/global/bp-global-energy-trading/features-and-updates/technical-downloads/crudes-assays.html"
html <- xml2::read_html(url)
x <- html %>% rvest::html_nodes("table") %>%
  rvest::html_table(fill=T) %>% .[[1]] %>%
  dplyr::as_tibble() %>% dplyr::slice(-1) %>% dplyr::select(1:5) %>%
  dplyr::transmute(Crude = X1, Country = X2, API = as.numeric(X3), Sulphur = as.numeric(X4), TAN = as.numeric(X5))

y <- cancrudeassayssum %>% dplyr::transmute(Crude,
                                       Country="Canada",
                                       API = Gravity, Sulphur,TAN) %>%
  dplyr::ungroup() %>% dplyr::select(-Ticker)

crudes <- rbind(x,y) %>%
  dplyr::mutate(SweetSour = case_when(Sulphur < 0.5 ~ "Sweet", TRUE ~ "Sour"),
                LightMedHeavy = case_when(API < 22.3 ~ "Heavy",
                                          API > 31.1 ~ "Light",
                                          TRUE ~ "Medium"))
crudes$LightMedHeavy <- factor(crudes$LightMedHeavy, levels=c("Light", "Medium", "Heavy"))
crudes$SweetSour <- factor(crudes$SweetSour, levels=c("Sweet", "Sour"))

usethis::use_data(crudes, overwrite = T)
rm(x,y)

## IR Curves for RQuantlib
  # Curves and Def
library(RTL)
fromDate = "2019-01-01" #Sys.Date() - months(36)
usSwapIR <- dplyr::tibble(tickQL = c("d1d","d1w","d1m","d3m","d6m","d1y",
                                     paste0("fut",1:8),
                                     paste0("s",c(2,3,5,7,10,15,20,30),"y")),
                          type = c(rep("ICE.LIBOR",6),rep("EuroDollar",8),rep("IRS",8)),
                          source = c(rep("FRED",6),rep("Morningstar",8),rep("FRED",8)),
                          tickSource = c("USDONTD156N","USD1WKD156N","USD1MTD156N","USD3MTD156N","USD6MTD156N","USD12MD156N",
                                         paste0("ED_",sprintf('%0.3d', 1:8),"_Month"),
                                         paste0("ICERATES1100USD",c(2,3,5,7,10,15,20,30),"Y")))

c = usSwapIR %>% dplyr::filter(source == "Morningstar") %>% .$tickSource
r <- getPrices(feed="CME_CmeFutures_EOD_continuous",contracts=c,from = fromDate,iuser = mstar[[1]], ipassword = mstar[[2]])
c = usSwapIR %>% dplyr::filter(source == "FRED") %>% .$tickSource
x <- tidyquant::tq_get(c, get  = "economic.data", from = fromDate ,to = as.character(Sys.Date())) %>%
  dplyr::mutate(price=price/100) %>%
  tidyr::pivot_wider(date,names_from = symbol, values_from = price)
r <- dplyr::left_join(x, r, by=c("date"))
colnames(r) <- c("date",dplyr::tibble(tickSource = colnames(r)[-1]) %>% dplyr::left_join(usSwapIR,by = c("tickSource")) %>% .$tickQL)
usSwapIRdef <- usSwapIR
usSwapIR <- r

  # Discount Objects

library(RQuantLib)
rates <- usSwapIR %>% stats::na.omit() %>% dplyr::filter(date == dplyr::last(date))
tradeDate <- rates$date
tsQuotes <- rates %>% dplyr::select(contains("d"),contains("fut"),contains("s"),-date,-d1d) %>%
  transpose() %>% unlist() %>% as.list()
params <- list(tradeDate = tradeDate, settleDate = tradeDate + 2, dt = 1/12,
               interpWhat="discount", interpHow="spline")
setEvaluationDate(tradeDate)
times <- seq(0,20,1/12)
savepar <- par(mfrow=c(3,3), mar=c(4,4,2,0.5))
on.exit(par(savepar))
#params$interpHow="spline"
usSwapCurves <- DiscountCurve(params, tsQuotes, times)
tsQuotes <- list(flat=0.03)
usSwapCurvesPar <- DiscountCurve(params, tsQuotes, times)

rm(r,x,rates,savepar,tsQuotes,params,mstar)

usethis::use_data(usSwapIR, overwrite = T)
usethis::use_data(usSwapIRdef, overwrite = T)
usethis::use_data(usSwapCurves, overwrite = T)
usethis::use_data(usSwapCurvesPar, overwrite = T)

devtools::document()
