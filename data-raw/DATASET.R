# usethis::use_readme_md()
# usethis::use_package("tibble")
# usethis::use_pipe()
library(tidyverse)
library(lubridate)
library(jsonlite)
source("~/keys.R")
setwd("~/RTL/data-raw")
## Prices Data (to migrate later)
  # library(RTL)
  # saveRDS(df_fut,"df_fut",compress = F)
  # saveRDS(dflong,"dflong",compress = F)
  # saveRDS(dfwide,"dfwide",compress = F)


df_fut <- readRDS("df_fut") ; usethis::use_data(df_fut, overwrite = T)
dflong <- readRDS("dflong") ; usethis::use_data(dflong, overwrite = T)
dfwide <- readRDS("dfwide") ; usethis::use_data(dfwide, overwrite = T)

ng_storage <- tibble::tribble(~ticker, ~series,"NG.NW2_EPG0_SWO_R48_BCF.W","NG Storage - Lower 48") %>%
  dplyr::mutate(key=EIAkey) %>%
  dplyr::mutate(df = purrr::pmap(list(ticker,key),.f=RTL::eia2tidy)) %>%
  dplyr::select(series, df) %>% tidyr::unnest()
usethis::use_data(ng_storage, overwrite = T)


# EIA Mapping
tickers_eia <- read.csv('eia.csv',sep=",",header=TRUE,na.strings="NA",stringsAsFactors=FALSE)
usethis::use_data(tickers_eia, overwrite = T)

## Futures Expiry Tables

  # library(readxl)
  # download.file(url="https://www.cmegroup.com/CmeWS/mvc/ProductCalendar/Download.xls?productId=425",
  #               destfile = "expiries")
  # expiries <- read_excel("expiries")
  # colnames(expiries) <- str_replace_all(string=colnames(expiries), pattern=" ", repl="")
  # write_csv2(expiries,"~/nymex_wti_expiries.csv")

futmonths = c("F","G","H","J","K","M","N","Q","U","V","X","Z")
expiry_table<-read.csv('expiry_table.csv',sep=",",header=TRUE,na.strings="NA",stringsAsFactors=FALSE)
expiry_table <- dplyr::mutate(expiry_table,
                              Last.Trade = as.Date(as.character(Last.Trade),"%Y/%m/%d",tz="UTC"),
                              First.Notice = as.Date(as.character(First.Notice),"%Y/%m/%d",tz="UTC"),
                              First.Delivery = as.Date(as.character(First.Delivery),"%Y/%m/%d",tz="UTC"),
                              Last.Delivery = as.Date(as.character(Last.Delivery),"%Y/%m/%d",tz="UTC"),
                              Year = year(First.Delivery),
                              Month = month(First.Delivery),
                              Month.Letter = futmonths[Month],
                              DataStream = case_when(cmdty=="cmewti" ~ paste("NCL",sprintf('%0.2d',Month),sprintf('%0.2d',Year-2000),sep=""),
                                                     cmdty=="cmeng" ~ paste("NNG",sprintf('%0.2d',Month),sprintf('%0.2d',Year-2000),sep=""),
                                                     cmdty=="icebrent" ~ paste("LLC",sprintf('%0.2d',Month),sprintf('%0.2d',Year-2000),sep=""),
                                                     cmdty=="cmeulsd" ~ paste("NHO",sprintf('%0.2d',Month),sprintf('%0.2d',Year-2000),sep=""),
                                                     cmdty=="cmerbob" ~ paste("NRB",sprintf('%0.2d',Month),sprintf('%0.2d',Year-2000),sep="")),
                              ticker = paste(tick.prefix,"_",Year,Month.Letter,sep="")
) %>% dplyr::filter(Year>2003)
usethis::use_data(expiry_table, overwrite = T)

## Holiday Calendar
holidaysOil<-read.csv('holidays.csv',sep=",",header=TRUE,na.strings="NA",stringsAsFactors=FALSE) %>%
  dplyr::mutate(nymex = as.Date(as.character(nymex),"%Y-%m-%d",tz="UTC"),
                ice = as.Date(as.character(ice),"%Y-%m-%d",tz="UTC")) %>% tidyr::gather()
holidaysOil <- holidaysOil[complete.cases(holidaysOil),]
usethis::use_data(holidaysOil, overwrite = T)

## tweets
  # http://www.trumptwitterarchive.com/archive
twtrump <- fromJSON("twtrump.json")
twtrump <- twtrump %>%
  dplyr::mutate(created=as.POSIXct(created_at,tz="GMT",format=c("%a %b %d %H:%M:%S +0000 %Y"))) %>%
  dplyr:::select(text,favoriteCount=favorite_count,created,id=id_str) %>%
  as_tibble()
usethis::use_data(twtrump, overwrite = T)

# library(twitteR)
# source(here::here("../dscf/packages.R"))
# setup_twitter_oauth(consumer_key = tw$cons.key, consumer_secret = tw$cons.secret,
#                     access_token = tw$access.token, access_secret = tw$access.secret)
# twoott <- twitteR::searchTwitter('#OOTT', n = 1e4, since = '2016-11-06', retryOnRateLimit = 1e4)
# twoott <- twitteR::twListToDF(twoott) %>% as_tibble()
# usethis::use_data(twoott, overwrite = T)

## Canadain Crude Data
cancrudeprices <- readRDS("~/dscf/data/crude_prices.RDS") ; usethis::use_data(cancrudeprices, overwrite = T)
cancrudeassays <- readRDS("~/dscf/data/crude_assays.RDS") ; usethis::use_data(cancrudeassays, overwrite = T)

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
