#' dataset: futures contracts metadata
#' @description Exchange-traded contract month codes and specifications.
#' @format data frame
"futuresRef"

#' dataset: EIA Short Term Energy Outlook
#' @description Short Term Energy Outlook from the EIA.
#' @format plotly object
#' @source {eia}
"steo"

#' dataset: WTI Cushing Futures and storage utilization
#' @description c1, c2, c1c2 and Cushing storage utilization
#' @format list
#' @source {CME and EIA}
"cushing"

#' dataset: randomiser to convert settlement into OHLC
#' @description OHLC profile using historical CL 1st Contract OHLC
#' @format data frame
#' @source {CME}
"ohlc"

#' dataset: Yahoo Finance data sets
#' @description Traded equity prices and returns
#' @format list
#' @source {Yahoo Finance}
"stocks"

#' dataset: spot to futures convergence
#' @description Cash and futures
#' @format data frame
#' @source {Morningstar, EIA}
"spot2futConvergence"

#' dataset: spot to futures convergence curve
#' @description Forward Curve
#' @format data frame
#' @source {Morningstar, EIA}
"spot2futCurve"

#' dataset: WTI Calendar Month Average Swap pricing data
#' @description WTI Crude futures
#' @format data frame
#' @source {Morningstar}
"wtiSwap"

#' dataset: USDCAD FX forward rates
#' @description USDCAD 1-year and 5-year forward points
#' @format data frame
#' @source {Morningstar}
"fxfwd"

#' dataset: Eurodollar futures contracts
#' @description ED futures contract for December 2024
#' @format data frame
#' @source {Morningstar}
"eurodollar"

#' dataset: IR compounding
#' @description Planet metrics from NASA
#' @format data frame
#' @source <https://nssdc.gsfc.nasa.gov/planetary/factsheet/index.html>
"planets"

#' dataset: expiry of common commodity futures contract.
#' @description This dataframe provides detailed information on major futures contracts specifications
#' pertaining to last settlement, notices and delivery dates. It also provides tickers in some data service.
#' @format data frame
"expiry_table"

#' dataset: NYMEX and ICE holiday calendars
#' @description Holiday calendars for NYMEX and ICE Brent
#' @format data frame
"holidaysOil"

#' datasest: metadata of key EIA tickers grouped by products.
#' @description Supports automated upload of EIA data through its API by categories.
#' Data frame organized by Supply Demand categories and products.
#' @format data frame
"tickers_eia"

#' dataset: EIA weekly stocks
#' @description EIA weekly crude, NG, ULSD and RBOB stocks.
#' @format data frame
"eiaStocks"

#' dataset: EIA working storage capacity
#' @description EIA working storage capacity in kbs except NG in bcf.
#' @format data frame
"eiaStorageCap"

#' dataset: crude assays
#' @description crude assays
#' @format list
"crudeOil"

#' dataset: commodity prices in a long dataframe format
#' @description Futures settlement data set.
#' @format data frame
#' @source Morningstar Commodities
"dflong"

#' dataset: commodity prices in a wide dataframe format
#' @description Futures settlement data set.
#' @format data frame
#' @source Morningstar Commodities
"dfwide"

#' dataset: US bootstrapped interest rate curve.
#' @description USD IR Discount, Forward and Zero curves from RQuantlib::DiscountCurve
#' @format List
#' #' @source Morningstar and FRED
"usSwapCurves"

#' dataset: US bootstrapped interest rate curve parallel sample.
#' @description USD IR Discount, Forward and Zero curves from RQuantlib::DiscountCurve - Parallel toy data set
#' @format data frame
"usSwapCurvesPar"

#' dataset: interest rate curve data for RQuantlib .
#' @description USD IR curve input for RQuantlib::DiscountCurve
#' @format data frame
"tsQuotes"

#' dataset: Canadian and US physical crude trading calendars
#' @description Crude Trading Trade Cycles
#' @format data frame
"tradeCycle"

#' dataset: GIS locations for crude oil trading hubs
#' @description Trading Hubs
#' @format data frame
"tradeHubs"

#' dataset: randomised physical crude differentials
#' @description Randomized data set for education purpose of selected physical crude differentials to WTI.
#' @format data frame
"fizdiffs"

#' dataset: refinery LP model sample inputs and outputs
#' @description Simple refinery  to be used in running LP modeling for education purposes.
#' @format list
"refineryLPdata"

#' dataset: data for teaching the various ways to monetize a market call.
#' @description Data set for explaining the various ways to monetize a market view.
#' @format data frame
"tradeprocess"
