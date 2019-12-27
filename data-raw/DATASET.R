## code to prepare `DATASET` dataset goes here
library(tidyverse)
library(lubridate)
library(jsonlite)

#
tickers_eia <- read.csv('eia.csv',sep=",",header=TRUE,na.strings="NA",stringsAsFactors=FALSE)
usethis::use_data(tickers_eia)

#
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
usethis::use_data(expiry_table)

#
holidaysOil<-read.csv('holidays.csv',sep=",",header=TRUE,na.strings="NA",stringsAsFactors=FALSE) %>%
  dplyr::mutate(nymex = as.Date(as.character(nymex),"%Y-%m-%d",tz="UTC"),
                ice = as.Date(as.character(ice),"%Y-%m-%d",tz="UTC"),
                ngx = as.Date(as.character(ngx),"%Y-%m-%d",tz="UTC")) %>% tidyr::gather()
holidaysOil <- holidaysOil[complete.cases(holidaysOil),]
usethis::use_data(holidaysOil)

# tweets http://www.trumptwitterarchive.com/archive
twtrump <- fromJSON("twtrump.json")
twtrump <- twtrump %>%
  dplyr::mutate(created=as.POSIXct(created_at,tz="GMT",format=c("%a %b %d %H:%M:%S +0000 %Y"))) %>%
  dplyr:::select(text,favoriteCount=favorite_count,created,id=id_str) %>%
  as_tibble()
usethis::use_data(twtrump)

library(twitteR)
source(here::here("../dscf/packages.R"))
setup_twitter_oauth(consumer_key = tw$cons.key, consumer_secret = tw$cons.secret,
                    access_token = tw$access.token, access_secret = tw$access.secret)
twoott <- twitteR::searchTwitter('#OOTT', n = 1e4, since = '2016-11-06', retryOnRateLimit = 1e4)
twoott <- twitteR::twListToDF(twoott) %>% as_tibble()
usethis::use_data(twoott)

#
cancrudeprices <- readRDS("~/dscf/data/crude_prices.RDS") ; usethis::use_data(cancrudeprices)
cancrudeassays <- readRDS("~/dscf/data/crude_assays.RDS") ; usethis::use_data(cancrudeassays)

# Prices Data (to migrate later)
# library(RTL)
# saveRDS(df_fut,"df_fut",compress = F)
# saveRDS(dflong,"dflong",compress = F)
# saveRDS(dfwide,"dfwide",compress = F)

df_fut <- readRDS("~/spd/data-raw/df_fut") ; usethis::use_data(df_fut)
dflong <- readRDS("~/spd/data-raw/dflong") ; usethis::use_data(dflong)
dfwide <- readRDS("~/spd/data-raw/dfwide") ; usethis::use_data(dfwide)



#
# usethis::use_readme_md()
# usethis::use_package("tibble")
# usethis::use_pipe()


