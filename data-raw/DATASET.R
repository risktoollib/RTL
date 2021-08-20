# usethis::use_pipe()
# usethis::use_readme_md()
# usethis::use_package("tidyverse")
usethis::use_package("tidyverse", type = "imports")
#usethis::use_package("Quandl","suggests")
#usethis::use_package("fitdistplus", "suggests")
#usethis::use_package("lpSolve", "suggests")
#usethis::use_package("rugarch", "suggests")
#usethis::use_package("PerformanceAnalytics", "suggests")

# Setup RTL Webpage
#usethis::use_pkgdown()
#usethis::use_github_actions("pkgdown")
#pkgdown::build_site()
#usethis::use_github_action(url = "https://raw.githubusercontent.com/r-lib/actions/master/examples/pkgdown.yaml")

#
library(RTL)
library(curl)
library(tidyverse)
library(lubridate)
library(jsonlite)
library(rvest)
library(readxl)
library(readr)
source("~/now/keys.R")
setwd(paste0(getwd(),"/data-raw"))

## WTI swaps
c <- paste0("CL0",c("M","N","Q"))
wtiSwap <- RTL::getPrices(feed = "CME_NymexFutures_EOD",
                       contracts = c,
                       from = "2019-08-26",
                       iuser = mstar[[1]], ipassword = mstar[[2]])

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

## FX forwards

fromDate <-  Sys.Date() - lubridate::years(1)
fxfwd <-
  RTL::getPrices(
    feed = "Morningstar_FX_Forwards",
    contracts = c("USDCAD 1Y", "USDCAD 5Y"),
    from = fromDate,
    iuser = mstar[[1]],
    ipassword = mstar[[2]]
  )

usethis::use_data(fxfwd, overwrite = T)

## Orbital

url = "https://nssdc.gsfc.nasa.gov/planetary/factsheet/index.html"
html <- xml2::read_html(url)

## Simplified tables
planets <- html %>% rvest::html_nodes("table") %>% rvest::html_table(header = TRUE) %>% .[[1]]
colnames(planets)[1] <- "Metric"
planets <- planets %>% dplyr::as_tibble() %>%
  dplyr::rename_all(.funs = stringr::str_to_title) %>%
  dplyr::mutate_all(function(x) gsub(",|\\*","",x)) %>%
  dplyr::mutate(dplyr::across(.cols = -Metric, .fns = as.numeric)) %>% na.omit() %>%
  tidyr::pivot_longer(-Metric,names_to = "Planet", values_to = "value") %>%
  tidyr::pivot_wider(names_from = Metric, values_from = value)
usethis::use_data(planets, overwrite = T)

## Sample energy futures datasets
df_fut <- readRDS("df_fut") ; usethis::use_data(df_fut, overwrite = T)

iuser = mstar[["iuser"]] ; ipassword = mstar[["ipassword"]]
startdate <- "2004-01-01"
crude <- c(paste0("CL_",sprintf('%0.3d', 1:36),"_Month"), paste0("NG_",sprintf('%0.3d', 1:36),"_Month"))
crudeICE <- c(paste0("BRN_",sprintf('%0.3d', 1:36),"_Month"))
pdts <- c(paste0("HO_",sprintf('%0.3d', 1:18),"_Month"), paste0("RB_",sprintf('%0.3d', 1:18),"_Month"))
crude <- RTL::getPrices(feed="CME_NymexFutures_EOD_continuous",
               contracts = crude,from = startdate,
               iuser = iuser, ipassword = ipassword) %>%
  pivot_longer(-date,names_to = "series", values_to = "value") %>%
  dplyr::mutate(series = stringr::str_replace_all(series,c("_0" = "","_Month" = ""))) %>% na.omit()
crudeICE <- RTL::getPrices(feed = "ICE_EuroFutures_continuous",
                        contracts = crudeICE,from = startdate,
                        iuser = iuser, ipassword = ipassword) %>%
  pivot_longer(-date,names_to = "series", values_to = "value") %>%
  dplyr::mutate(series = stringr::str_replace_all(series,c("_0" = "","_Month" = ""))) %>% na.omit()
pdts <- RTL::getPrices(feed = "CME_NymexFutures_EOD_continuous",
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
eiaStocks <- tibble::tribble(~ticker, ~name,
                         "PET.W_EPC0_SAX_YCUOK_MBBL.W", "CrudeCushing",
                         "PET.WGTSTUS1.W", "Gasoline",
                         "PET.WD0ST_NUS_1.W", "ULSD",
                         "NG.NW2_EPG0_SWO_R48_BCF.W","NGLower48") %>%
  dplyr::mutate(key = EIAkey) %>%
  dplyr::mutate(df = purrr::pmap(list(ticker,key,name),.f = RTL::eia2tidy)) %>%
  dplyr::select(df) %>% tidyr::unnest(df)
usethis::use_data(eiaStocks, overwrite = T)

## EIA Storage Capacity
url <- "https://www.eia.gov/petroleum/storagecapacity/crudeoilstorage.xlsx"
destfile <- "crudeoilstorage.xlsx"
curl::curl_download(url, destfile)
name <- "Refinery and Tank and Underground Working Storage Capacity"

cc <- function(name = "Refinery and Tank and Underground Working Storage Capacity", sheet = "US", loc = "US") {
  tmp <- read_excel(destfile, skip = 3, sheet = sheet) %>% dplyr::filter(.[[1]] == name) %>%
    dplyr::rename(series = "...1") %>% dplyr::mutate(series = loc)
  colnames(tmp) <- c("series",as.character(as.Date(as.numeric(colnames(tmp)[-1]),origin = "1899-12-30")))
  tmp <- tmp %>% tidyr::pivot_longer(-series,names_to = "date", values_to = "value") %>%
    dplyr::mutate(date = as.Date(date),value = as.numeric(value))
  return(tmp)
}

eiaStorageCap <- bind_rows(cc(sheet = "US", loc = "US"),
          cc(sheet = "PADD 1", loc = "P1"),
          cc(sheet = "PADD 2", loc = "P2"),
          cc(sheet = "PADD 3", loc = "P3"),
          cc(sheet = "PADD 4", loc = "P4"),
          cc(sheet = "PADD 5", loc = "P5"),
          cc(name = "Tank Working Storage Capacity", sheet = "Cushing", loc = "Cushing"))
file.remove(destfile)
usethis::use_data(eiaStorageCap, overwrite = T)

## Sample GIS Mapping
library(rgdal)

crudepipelines <- rgdal::readOGR(dsn = "~/now/RTL/GIS_EIA/CrudeOil_Pipelines_US_EIA/", layer = "CrudeOil_Pipelines_US_202001")
refineries <- rgdal::readOGR(dsn = "~/now/RTL/GIS_EIA/Petroleum_Refineries_US_EIA/", layer = "Petroleum_Refineries_US_2020")
productspipelines <- rgdal::readOGR(dsn = "~/now/RTL/GIS_EIA/PetroleumProduct_Pipelines_US_EIA/", layer = "PetroleumProduct_Pipelines_US_202001")
productsterminals <- rgdal::readOGR(dsn = "~/now/RTL/GIS_EIA/PetroleumProduct_Terminals_US_EIA/", layer = "PetroleumProduct_Terminals_US_202001")
ngpipelines <- rgdal::readOGR(dsn = "~/now/RTL/GIS_EIA/NaturalGas_InterIntrastate_Pipelines_US_EIA/", layer = "NaturalGas_Pipelines_US_202001")
ngstorage <- rgdal::readOGR(dsn = "~/now/RTL/GIS_EIA/NaturalGas_StorageRegions_US_EIA/", layer = "NaturalGas_StorageRegions_20151119")
nghubs <- rgdal::readOGR(dsn = "~/now/RTL/GIS_EIA/NaturalGas_TradingHubs_US_EIA/", layer = "NaturalGas_TradingHubs_US_202002")
lngterminals <- rgdal::readOGR(dsn = "~/now/RTL/GIS_EIA/Lng_ImportExportTerminals_US_EIA/", layer = "LNG_ImpExp_Terminals_US_202004")

#usethis::use_data(abOilNG, overwrite = T)
usethis::use_data(crudepipelines, overwrite = T)
usethis::use_data(refineries, overwrite = T)
usethis::use_data(productspipelines, overwrite = T)
usethis::use_data(productsterminals, overwrite = T)
usethis::use_data(ngpipelines, overwrite = T)
usethis::use_data(ngstorage, overwrite = T)
usethis::use_data(nghubs, overwrite = T)
usethis::use_data(lngterminals, overwrite = T)

## EIA Mapping
tickers_eia <- read.csv('./data-raw/eia.csv',sep = ",",header = TRUE,na.strings = "NA",stringsAsFactors = FALSE)
usethis::use_data(tickers_eia, overwrite = T)

# Expiry table
futmonths = c("F","G","H","J","K","M","N","Q","U","V","X","Z")
expiry_table <- read.csv('expiry_table.csv',sep = ",",header = TRUE,na.strings = "NA",stringsAsFactors = FALSE) %>%
  dplyr::as_tibble() %>%
  dplyr::mutate(Last.Trade = as.Date(Last.Trade,"%Y-%m-%d",tz = "UTC"),
                First.Notice = as.Date(First.Notice,"%Y-%m-%d",tz = "UTC"),
                First.Delivery = as.Date(First.Delivery,"%Y-%m-%d",tz = "UTC"),
                Last.Delivery = as.Date(Last.Delivery,"%Y-%m-%d",tz = "UTC"))


LGO <- read_csv("https://www.theice.com/api/productguide/spec/34361119/expiry/csv",
                col_types = cols(`CONTRACT SYMBOL` = col_skip(),
                                 FTD = col_skip(), LTD = col_date(format = "%m/%d/%Y"),
                                 FND = col_date(format = "%m/%d/%Y"),
                                 LND = col_skip(), FDD = col_date(format = "%m/%d/%Y"),
                                 LDD = col_date(format = "%m/%d/%Y"),
                                 FSD = col_skip(), `OPTIONS FTD` = col_skip(),
                                 `OPTIONS LTD` = col_skip())) %>%
  dplyr::transmute(cmdty = "icegasoil", tick.prefix = "LGO", Last.Trade = LTD,
                   First.Notice = FND, First.Delivery = FDD, Last.Delivery = LDD)

LCO <- read_csv("https://www.theice.com/api/productguide/spec/219/expiry/csv",
                col_types = cols(`CONTRACT SYMBOL` = col_skip(),
                                 FTD = col_skip(), LTD = col_date(format = "%m/%d/%Y"),
                                 FND = col_date(format = "%m/%d/%Y"),
                                 LND = col_skip(), FDD = col_date(format = "%m/%d/%Y"),
                                 LDD = col_date(format = "%m/%d/%Y"),
                                 FSD = col_skip(), `OPTIONS FTD` = col_skip(),
                                 `OPTIONS LTD` = col_skip())) %>%
  dplyr::transmute(cmdty = "icebrent", tick.prefix = "LCO", Last.Trade = LTD,
                   First.Notice = FND,
                   First.Delivery = lubridate::rollback(First.Notice,roll_to_first = TRUE) + months(2),
                   Last.Delivery = lubridate::rollback(First.Delivery + months(1)))

#destfile <- "Download.xls"
#curl::curl_download(url = "https://www.cmegroup.com/CmeWS/mvc/ProductCalendar/Download.xls?productId=425",destfile)
CL <- read_excel("CL.xls", col_types = c("skip", "skip", "skip", "text", "skip", "skip",
                                         "skip", "skip", "skip", "text", "skip", "text", "text")) %>%
  dplyr::as_tibble(.name_repair = "universal") %>%
  dplyr::transmute(cmdty = "cmewti", tick.prefix = "CL",
                   Last.Trade = as.Date(Last.Trade,"%Y-%m-%d",tz = "UTC"),
                   First.Notice = as.Date(First.Notice,"%Y-%m-%d",tz = "UTC"),
                   First.Delivery = as.Date(First.Delivery,"%Y-%m-%d",tz = "UTC"),
                   Last.Delivery = as.Date(Last.Delivery,"%Y-%m-%d",tz = "UTC"))

#curl::curl_download(url = "https://www.cmegroup.com/CmeWS/mvc/ProductCalendar/Download.xls?productId=424",destfile)

BZ <- read_excel("BZ.xls", col_types = c("skip", "skip", "skip", "text", "text", "skip",
                                         "skip", "skip", "skip", "text", "skip","text", "text")) %>%
  dplyr::as_tibble(.name_repair = "universal") %>%
  dplyr::transmute(cmdty = "cmebrent", tick.prefix = "BZ",
                   Last.Trade = as.Date(Last.Trade,"%Y-%m-%d",tz = "UTC"),
                   First.Notice = as.Date(Settlement,"%Y-%m-%d",tz = "UTC"),
                   First.Delivery = lubridate::rollback(First.Notice, roll_to_first = TRUE) + months(2),
                   Last.Delivery = First.Delivery)

#curl::curl_download(url = "https://www.cmegroup.com/CmeWS/mvc/ProductCalendar/Download.xls?productId=426",destfile)
HO <- read_excel("HO.xls", col_types = c("skip", "skip", "skip", "text", "skip", "skip",
                                         "skip", "skip", "skip", "text", "skip", "text", "text")) %>%
  dplyr::as_tibble(.name_repair = "universal") %>%
  dplyr::transmute(cmdty = "cmeulsd", tick.prefix = "HO",
                   Last.Trade = as.Date(Last.Trade,"%Y-%m-%d",tz = "UTC"),
                   First.Notice = as.Date(First.Notice,"%Y-%m-%d",tz = "UTC"),
                   First.Delivery = as.Date(First.Delivery,"%Y-%m-%d",tz = "UTC"),
                   Last.Delivery = as.Date(Last.Delivery,"%Y-%m-%d",tz = "UTC"))


#curl::curl_download(url = "https://www.cmegroup.com/CmeWS/mvc/ProductCalendar/Download.xls?productId=429",destfile)
RB <- read_excel("RB.xls", col_types = c("skip", "skip", "skip", "text", "skip", "skip",
                                         "skip", "skip", "skip", "text", "skip", "text", "text"))  %>%
  dplyr::as_tibble(.name_repair = "universal") %>%
  dplyr::transmute(cmdty = "cmerbob", tick.prefix = "RB",
                   Last.Trade = as.Date(Last.Trade,"%Y-%m-%d",tz = "UTC"),
                   First.Notice = as.Date(First.Notice,"%Y-%m-%d",tz = "UTC"),
                   First.Delivery = as.Date(First.Delivery,"%Y-%m-%d",tz = "UTC"),
                   Last.Delivery = as.Date(Last.Delivery,"%Y-%m-%d",tz = "UTC"))

GC <- read_excel("GC.xls", col_types = c("skip", "skip", "skip", "text", "skip", "skip",
                                         "skip", "skip", "skip", "text", "skip", "text", "text"))  %>%
  dplyr::as_tibble(.name_repair = "universal") %>%
  dplyr::transmute(cmdty = "comexgold", tick.prefix = "GC",
                   Last.Trade = as.Date(Last.Trade,"%Y-%m-%d",tz = "UTC"),
                   First.Notice = as.Date(First.Notice,"%Y-%m-%d",tz = "UTC"),
                   First.Delivery = as.Date(First.Delivery,"%Y-%m-%d",tz = "UTC"),
                   Last.Delivery = as.Date(Last.Delivery,"%Y-%m-%d",tz = "UTC"))

SI <- read_excel("SI.xls", col_types = c("skip", "skip", "skip", "text", "skip", "skip",
                                         "skip", "skip", "skip", "text", "skip", "text", "text"))  %>%
  dplyr::as_tibble(.name_repair = "universal") %>%
  dplyr::transmute(cmdty = "comexsilver", tick.prefix = "SI",
                   Last.Trade = as.Date(Last.Trade,"%Y-%m-%d",tz = "UTC"),
                   First.Notice = as.Date(First.Notice,"%Y-%m-%d",tz = "UTC"),
                   First.Delivery = as.Date(First.Delivery,"%Y-%m-%d",tz = "UTC"),
                   Last.Delivery = as.Date(Last.Delivery,"%Y-%m-%d",tz = "UTC"))

ALI <- read_excel("ALI.xls", col_types = c("skip", "skip", "skip", "text", "skip", "skip",
                                         "skip", "skip", "skip", "text", "skip", "text", "text"))  %>%
  dplyr::as_tibble(.name_repair = "universal") %>%
  dplyr::transmute(cmdty = "comexalu", tick.prefix = "ALI",
                   Last.Trade = as.Date(Last.Trade,"%Y-%m-%d",tz = "UTC"),
                   First.Notice = as.Date(First.Notice,"%Y-%m-%d",tz = "UTC"),
                   First.Delivery = as.Date(First.Delivery,"%Y-%m-%d",tz = "UTC"),
                   Last.Delivery = as.Date(Last.Delivery,"%Y-%m-%d",tz = "UTC"))

expiry_table <- rbind(expiry_table,LCO,LGO,CL,BZ,HO,RB,GC,SI,ALI) %>%
  dplyr::distinct() %>%
  dplyr::mutate(Year = year(First.Delivery),
                Month = month(First.Delivery),
                Month.Letter = futmonths[Month],
                yahoo.ticker = paste0(tick.prefix,
                                      Month.Letter,
                                      str_sub(Year, start = 3L, end = 4L), ".",
                                      dplyr::case_when(tick.prefix %in% c("CL","","RB","HO","NG") ~ "NYM",
                                                       tick.prefix %in% c("GC","SI","ALI")  ~ "CMX",
                                                       TRUE ~ "NA")),
                yahoo.ticker = ifelse(grepl("NA",yahoo.ticker),NA,yahoo.ticker)) %>%
  dplyr::filter(Year > 2003)
usethis::use_data(expiry_table, overwrite = T)

## Holiday Calendar
holidaysOil <- read.csv('holidays.csv',sep=",",header=TRUE,na.strings="NA",stringsAsFactors=FALSE) %>%
  dplyr::mutate(nymex = as.Date(as.character(nymex),"%Y-%m-%d",tz="UTC"),
                ice = as.Date(as.character(ice),"%Y-%m-%d",tz="UTC")) %>% tidyr::gather()
holidaysOil <- holidaysOil[complete.cases(holidaysOil),]
usethis::use_data(holidaysOil, overwrite = T)

## tradeCycle
tradeCycle <- read.csv('tradeCycle.csv',sep=",",header=TRUE,na.strings="NA",stringsAsFactors=FALSE)
usethis::use_data(tradeCycle, overwrite = T)

## Canadain Crude Data

url <-  "https://crudemonitor.ca/crudes/index.php?acr=MSW"
html <- xml2::read_html(url)

get_all_assays<-function(x) {
  print(x)
  t<-try(read_html(paste0("http://www.crudemonitor.ca/",x)) %>%  html_nodes("a") %>% rvest::html_attr("href") %>%
           data.frame(baselocation=.) %>% dplyr::filter(grepl("date=",baselocation)))
  if(!class(t)=="try-error"){
    return(read_html(paste0("http://www.crudemonitor.ca/",x)) %>%  html_nodes("a") %>% rvest::html_attr("href") %>%
             data.frame(baselocation=.) %>% dplyr::filter(grepl("date=",baselocation)))
  } else { return(data.frame(baselocation=character()))}
}
#x<-"sampledata.php?name=Cochin+Condensate&batch=CHN-010&date=2016-10-05"
get_assay_values<-function(x){
  print(x)
  tmp<-read_html(paste0("http://www.crudemonitor.ca/",x)) %>%
    html_nodes('#datatables') %>% html_text() %>% stringr::str_split("\\n",simplify = T)
  out<-data.frame(measure=tmp[1,grep("\\S+\\s\\(",tmp[1,])],value=tmp[1,grep("\\S+\\s\\(",tmp[1,])+1])
  return(out)
}

assay_id <-read_html("http://www.crudemonitor.ca/report.php") %>%
  rvest::html_nodes("a") %>% rvest::html_attr("href") %>% data.frame(baselocation=.) %>%
  dplyr::filter(grepl("report.php\\?acr=",baselocation)) %>%
  setNames("shortname") %>%
  dplyr::filter(grepl("=AWB|=BRN|=CDB|=CL|=LLB|=LLK|=MSW|=WCS|=WH",shortname)) %>%
  arrange %>%
  dplyr::mutate(longname = purrr::map(shortname,get_all_assays)) %>% tidyr::unnest(longname) %>%
  tidyr::separate(shortname,into = c("j1","Ticker"),sep = "acr=") %>%
  dplyr::mutate(filelocation = baselocation) %>%
  tidyr::separate(baselocation,into = c("j2","j3","Batch","Date","junk"),sep="=") %>%
  dplyr::mutate(Crude = gsub("&batch","",j3)) %>%
  dplyr::mutate(Crude = gsub("\\+"," ",Crude)) %>%
  dplyr::select(Ticker,Date,Batch,filelocation,Crude)

# Scrape Assay Content by ID
assays <- assay_id %>% na.omit() %>%
  dplyr::mutate(data = purrr::map(filelocation,get_assay_values)) %>%
  unnest(data)

# Tidying Data
cancrudeassays <- assays %>%
  dplyr::mutate(Ticker = gsub("\\&.*","",Ticker),
                Date = as.Date(str_remove(Date,"&PHPSESSID")),
                Batch = gsub("\\&.*","",Batch),
                Measurement = gsub("\\s\\(.*","",measure),
                Value = parse_number(as.character(value),na=c("","NA","ND"))) %>%
  dplyr::select(Date,Batch,Ticker,Crude,Measurement,Value) %>%
  stats::na.omit() %>%
  dplyr::filter(Ticker != "MSW(S)")

# Computing Monthly Measurements Averages

cancrudeassays <- cancrudeassays %>%
  pivot_wider(names_from = "Measurement",values_from = "Value") %>%
  dplyr::mutate(date = tsibble::yearmonth(Date),
                Location = case_when(grepl("AHS|AWB|C5|CAL|MSW|PSO|WDB|WH|KDB",Ticker)  ~ "Edmonton",
                                   grepl("BRN|CDB|CL|LLB|LLK|WCS",Ticker)  ~ "Hardisty",
                                   TRUE ~ "unknown")) %>%
  dplyr::select(date,Location,everything(),-contains("%")) %>%
  group_by(Ticker,Crude,date,Location) %>%
  summarise_if(is.numeric, mean, na.rm = TRUE)

cancrudeassays <-  cancrudeassays %>% dplyr::ungroup() %>% dplyr::arrange(Ticker, date) %>%
  dplyr::mutate(TAN = replace(TAN, is.nan(TAN), NA),
                TAN = case_when((is.na(TAN) & Ticker %in% c("MSW", "SYN")) ~ 0,
                                TRUE ~ TAN)) %>%
  tidyr::fill(TAN)

usethis::use_data(cancrudeassays, overwrite = T)

cancrudeassayssum <- cancrudeassays %>% dplyr::group_by(Ticker,Crude) %>%
  #dplyr::filter(YM > "2015-01-01", Ticker != "MSW(S)") %>%
  dplyr::select(-Location,-Sediment,-Salt,-Olefins,-Viscosity) %>%
  dplyr::mutate(TAN = case_when(Ticker == "MSW" ~ 0,TRUE ~ TAN)) %>%
  na.omit() %>% summarise_all(list(mean))

usethis::use_data(cancrudeassayssum, overwrite = T)

cancrudeprices <- readRDS("crude_prices.RDS") %>%
  dplyr::transmute(Ticker = Ticker, date = tsibble::yearmonth(YM), Value = Value)
usethis::use_data(cancrudeprices, overwrite = T)

# BP Assays
### capline https://cappl.com/Reports1.aspx
library(rvest)
url = "https://www.bp.com/en/global/bp-global-energy-trading/features-and-updates/technical-downloads/crudes-assays.html"
html <- xml2::read_html(url)

## Simplified tables
x <- html %>% rvest::html_nodes("table") %>%
  rvest::html_table(fill=T) %>% .[[1]] %>%
  dplyr::as_tibble() %>% dplyr::slice(-1) %>% dplyr::select(1:5) %>%
  dplyr::transmute(Crude = X1, Country = X2, API = as.numeric(X3), Sulphur = as.numeric(X4), TAN = as.numeric(X5))

y <- cancrudeassayssum %>% dplyr::transmute(Crude,
                                       Country="Canada",
                                       API = Gravity, Sulphur,TAN) %>%
  dplyr::ungroup() %>% dplyr::select(-Ticker)

crudes <- rbind(x,y) %>%
  tibble::add_row(Crude = "Tapis", Country = "Malaysia", API = 42.7, Sulphur = .044, TAN = 0.215) %>%
  tibble::add_row(Crude = "Maya", Country = "Mexico", API = 22, Sulphur = 3.3, TAN = 0.3) %>%
  tibble::add_row(Crude = "Light Louisiana Sweet", Country = "US", API = 38.5, Sulphur = .40, TAN = 0.25) %>%
  tibble::add_row(Crude = "West Texas Intermediate", Country = "US", API = 40.6, Sulphur = .22, TAN = 0.1)

crudes <- crudes %>%
  dplyr::mutate(SweetSour = case_when(Sulphur < 0.5 ~ "Sweet", TRUE ~ "Sour"),
                LightMedHeavy = case_when(API < 22.3 ~ "Heavy",
                                          API > 31.1 ~ "Light",
                                          TRUE ~ "Medium"),
                Benchmark = if_else(Crude %in% c("Maya",#"Oriente","Napo",
                                               "West Texas Intermediate","Light Louisiana Sweet",
                                               "Oman Export Blend","Mars",
                                               "Brent","Tapis"),"yes","no"),
                Notes = case_when(Country == "Canada" ~ "Canada",
                                          Benchmark == "yes" ~ "Benchmark",
                                          TRUE ~ "Others"),)

crudes$LightMedHeavy <- factor(crudes$LightMedHeavy, levels=c("Light", "Medium", "Heavy"))
crudes$SweetSour <- factor(crudes$SweetSour, levels=c("Sweet", "Sour"))
usethis::use_data(crudes, overwrite = T)
rm(x,y)

## xls assays
css <- "body > div.aem-Grid.aem-Grid--12.aem-Grid--default--12 > div:nth-child(3) > div > div > div.nr-table-component.nr-component.aem-GridColumn.aem-GridColumn--default--12"
urls <- html %>% html_nodes(css = css) %>%
  html_nodes("a") %>% rvest::html_attr("href") %>%
  as_tibble() %>%
  dplyr::transmute(xls = paste0("https://www.bp.com",value)) %>% unique() %>%
  dplyr::mutate(cn = str_replace_all(xls,
                                     c("https://www.bp.com/content/dam/bp/business-sites/en/global/bp-global-energy-trading/documents/what-we-do/crudes/" = "",".xls" = "")))

crudeassaysBP <- list()
for (i in 1:nrow(urls)){
  destfile = urls$cn[i]
  curl::curl_download(url = urls$xls[i],
                      destfile = destfile)
  tmp <- readxl::read_excel(destfile, range = "B30:P85", col_names = F)
  colnames(tmp) <- c("Specification","Whole.crude","Light.Naphtha","Heavy.Naphtha1","Heavy.Naphtha2","Kero","Light.Gas.Oil","HeavyGasOil","Light.VGO","Heavy.VGO1","Heavy.VGO2","AtRes1","AtRes2","VacRes1","VacRes2")
  tmp <- tmp %>%
    tidyr::drop_na("Specification") %>%
    dplyr::na_if('-') %>%
    dplyr::mutate_at(vars(!starts_with("Spec")),as.numeric) %>%
    as_tibble()
  crudeassaysBP[[destfile]] <- tmp
  file.remove(destfile)
}

usethis::use_data(crudeassaysBP, overwrite = T)

# Exxon Assays

url = "https://corporate.exxonmobil.com/Crude-oils/Crude-trading/Crude-oil-blends-by-API-gravity-and-by-sulfur-content#APIgravity"
html <- xml2::read_html(url)
css <- "body > main > div.article--wrapper > section.rich-text > div > div"

urls <- html %>% html_nodes(css = css) %>%
  html_nodes("a") %>% rvest::html_attr("href") %>%
  as_tibble() %>% dplyr::filter(grepl("/Crude-oils/",value)) %>%
  dplyr::transmute(site = paste0("https://corporate.exxonmobil.com",value)) %>% unique()

fetchURL <- function(url, css, prefix){
  xml2::read_html(url) %>% html_nodes(css = css) %>%
    html_nodes("a") %>% rvest::html_attr("href") %>%
    paste0(prefix,.)
}

urls <- urls %>% dplyr::mutate(prefix = "https://corporate.exxonmobil.com",
                       xls = mapply(fetchURL,url = site,
                                      css = "body > main > div.article--wrapper > section.articleMedia.articleMedia-in-line.articleMedia--relatedContent > div > article > div > div:nth-child(3) > h3",
                                      prefix = prefix)) %>%
  dplyr::select(-prefix) %>% dplyr::filter(!grepl(".pdf",xls,ignore.case = T)) %>%
  dplyr::mutate(cn = gsub("/.*","",gsub("https://corporate.exxonmobil.com/-/media/Global/Files/crude-oils/","",xls)))

crudeassaysXOM <- list()
for (i in 1:nrow(urls)){
  destfile = urls$cn[i]
  curl::curl_download(url = urls$xls[i],
                      destfile = destfile)
  tmp <- read_excel(destfile, skip = 4) %>%
    dplyr::rename_all(list(~make.names(.)))
  colnames(tmp)[1] <- destfile
  crudeassaysXOM[[destfile]] <- tmp
  file.remove(destfile)
}

usethis::use_data(crudeassaysXOM, overwrite = T)
rm(html,tmp,urls,css,destfile,i)

## Physical Diffs
fizdiffs <-  readRDS("fizdiffs.RDS")
usethis::use_data(fizdiffs, overwrite = T)


## IR Curves for RQuantlib
  # Curves and Def - ICE
library(Quandl)
Quandl::Quandl.api_key(quandlkey)
fromDate = Sys.Date() - months(1)
usSwapIR <- dplyr::tibble(tickQL = c("d1d","d1w","d1m","d3m","d6m","d1y",
                                     paste0("fut",1:8),
                                     paste0("s",c(2,3,5,7,10,15,20,30),"y")),
                          type = c(rep("ICE.LIBOR",6),rep("EuroDollar",8),rep("IRS",8)),
                          source = c(rep("FRED",6),rep("quandl",8),rep("FRED",8)),
                          tickSource = c("USDONTD156N","USD1WKD156N","USD1MTD156N","USD3MTD156N","USD6MTD156N","USD12MD156N",
                                         paste0("CHRIS/CME_ED",sprintf('%0.1d', 1:8)),
                                         paste0("ICERATES1100USD",c(2,3,5,7,10,15,20,30),"Y")))
#c = usSwapIR %>% dplyr::filter(source == "Morningstar") %>% .$tickSource
#r <- getPrices(feed="CME_CmeFutures_EOD_continuous",contracts=c,from = fromDate,iuser = mstar[[1]], ipassword = mstar[[2]])
c = usSwapIR %>% dplyr::filter(source == "quandl") %>% .$tickSource
r <- tidyquant::tq_get(x = c, get = "quandl",from = fromDate ,to = as.character(Sys.Date())) %>%
  #dplyr::select(symbol,date,settle) %>% dplyr::mutate(price = 100 - settle) %>%
  dplyr::select(symbol,date,settle) %>% dplyr::mutate(price = settle) %>%
  tidyr::pivot_wider(date,names_from = symbol, values_from = price)
c = usSwapIR %>% dplyr::filter(source == "FRED") %>% .$tickSource
x <- tidyquant::tq_get(c, get  = "economic.data", from = fromDate ,to = as.character(Sys.Date())) %>%
  dplyr::mutate(price=price/100) %>%
  tidyr::pivot_wider(date,names_from = symbol, values_from = price)
r <- dplyr::left_join(x, r, by=c("date"))
colnames(r) <- c("date",dplyr::tibble(tickSource = colnames(r)[-1]) %>% dplyr::left_join(usSwapIR,by = c("tickSource")) %>% .$tickQL)
usSwapIRdef <- usSwapIR %>% stats::na.omit()
usSwapIR <- r %>% stats::na.omit()

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
usSwapCurves <- DiscountCurve(params, tsQuotes, times)
tsQuotes <- list(flat=0.03)
usSwapCurvesPar <- DiscountCurve(params, tsQuotes, times)

rm(r,x,rates,savepar,tsQuotes,params,mstar)

usethis::use_data(usSwapIR, overwrite = T)
usethis::use_data(usSwapIRdef, overwrite = T)
usethis::use_data(usSwapCurves, overwrite = T)
usethis::use_data(usSwapCurvesPar, overwrite = T)

# Refinery Optimization

ref.opt.inputs <- data.frame(info=c("price","processing.fee"),
                     LightSweet=c(-50,-1),
                     HeavySour=c(-30,-4))

ref.opt.outputs <- data.frame(product=c("mogas","distillate","fo","resid"),
                       prices=c(70,70,30,20),
                       max.prod=c(50000,40000,20000,10000),
                       LightSweet.yield=c(0.65,.20,.10,0.05),
                       HeavySour.yield=c(0.40,0.20,.30,.1))

usethis::use_data(ref.opt.inputs, overwrite = T)
usethis::use_data(ref.opt.outputs, overwrite = T)

# Educational Dataset

tradeprocess <- RTL::getPrices(feed="CME_NymexFutures_EOD",contracts = c("@CL21H","@HO1F","@HO21H","@LT21H"),
                               from="2018-01-01",iuser = mstar[[1]], ipassword = mstar[[2]])
usethis::use_data(tradeprocess, overwrite = T)

# Global
devtools::document()










