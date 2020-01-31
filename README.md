## Purpose

+ Purposely designed functions for trading, trading analytics and risk practitioners in in Commodities and Finance. 
+ Build to support delivery of Finance classes from one of the co-authors of RTL at the [Alberta School of Business](https://www.ualberta.ca/business).
+ Data set for an upcoming accompanying book **Practical Data Science for Trading Risk Management
Professionals** for 2020 publication by [Risk Books](https://riskbooks.com/).

## Features

+ Historical forward curves charting.
+ Calendars and expiry dates data objects for a wide range of commodity futures contracts.
+ Function to adjust continuous contracts returns for roll adjustments using expiries above.
+ [Morningstar Marketplace API](https://mp.morningstarcommodity.com/marketplace/) functions `getPrice()` and `getPrices()` using your own Morningstar credentials. Current feeds included:

  + ICE_EuroFutures and ICE_EuroFutures_continuous.
  + CME_NymexFutures_EOD and CME_NymexFutures_EOD_continuous.
  + CME_CbotFuturesEOD and CME_CbotFuturesEOD_continuous.
  + CME_CmeFutures_EOD and CME_CmeFutures_EOD_continuous.
  + CME_STLCPC_Futures.
  + ICE_NybotCoffeeSugarCocoaFutures and ICE_NybotCoffeeSugarCocoaFutures_continuous.
  + Morningstar_FX_Forwards.
  + ... see `?getPrice` for up to date selection and examples.
+ `chart_zscore()` supports seasonality adjusted analysis of residuals, particularly useful when dealing with commodity stocks and/or days demand time series with trends as well as non-constant variance across seasonal periods.

## Data Sets

Accessible via `data(datsetname)`

+ `expiry_table`: Historical and forward futures contract metadata.
+ `holidaysOil`: Holiday calendars for ICE and NYMEX.
+ `tickers_eia`: Mapping of EIA tickers to crude and refined products markets for building supply demand balances.
+ `usSwapIRDef`: Data frame of definitions for instruments to build a curve for use with `RQuantlib`. Use `getIRswapCurve()` to extract the latest data from `FRED` and `Morningstar`.
+ `usSwapIR`: Toy data set output of `getIRswapCurve`.
+ `usSwapCurves`: Toy data set output of `RQuantlib::DiscountCurve()`.
+ `twtrump` and `twoott` are historical tweets toy data sets from @realDonaldTrump and #OOTT for learning NLP.
+ `cancrudeassays` contains historical Canadian crude assays from [Crudemonitor](https://crudemonitor.ca/home.php) as a learning and testing data set.

## Python

A python wrapper for some functions is available at https://github.com/bbcho/pyRTL

## Installation

library(devtools)

devtools::install_github("risktoollib/RTL")

library(RTL)

## Credentials

Usernames and password for API services are required. 





