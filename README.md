## Purpose

+ Purposely designed functions for trading, trading analytics and risk practitioners in in Commodities and Finance. 
+ Build to support delivery of Finance classes from one of the co-authors of RTL at the [Alberta School of Business](https://www.ualberta.ca/business).
+ Data set for an upcoming accompanying book **Practical Data Science for Trading Risk Management
Professionals** for 2020 publication by [Risk Books](https://riskbooks.com/).

## Features

+ Historical forward curves charting.
+ Calendars and expiry dates for a wide range of commodity futures contracts.
+ Function to adjust continuous contracts returns for roll adjustments using expiries above.
+ [Morningstar Marketplace API](https://mp.morningstarcommodity.com/marketplace/) functions `getPrice()` and `getPrices()` using your own Morningstar credentials. Current feeds included:

  + ICE_EuroFutures and ICE_EuroFutures_continuous.
  + CME_NymexFutures_EOD and CME_NymexFutures_EOD_continuous.
  + CME_CbotFuturesEOD and CME_CbotFuturesEOD_continuous.
  + CME_CmeFutures_EOD and CME_CmeFutures_EOD_continuous.
  + CME_STLCPC_Futures.
  + ICE_NybotCoffeeSugarCocoaFutures and ICE_NybotCoffeeSugarCocoaFutures_continuous.
  + Morningstar_FX_Forwards.
  

## Data Sets

+ `expiry_table` contains historical and forward futures contract metadata.
+ `tickers_eia` contains a mapping of EIA tickers to crude and refined products markets for building supply demand balances.
+ `twtrump` and `twoott` are historical tweets toy data sets from @realDonaldTrump and #OOTT for learning NLP.
+ `cancrudeassays` contains historical Canadian crude assays from [Crudemonitor](https://crudemonitor.ca/home.php) as a learning and testing data set.

## Python

A python wrapper for some functions is available at https://github.com/bbcho/pyRTL

## Installation

library(devtools)

devtools::install_github("risktoollib/RTL")

library(RTL)

## Credentials

Usernames and password for API services are hidden. For the purpose of examples the first line `source("~/keys.R")` needs to be replaced with your own.





