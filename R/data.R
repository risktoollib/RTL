#' Sample SPY ETF data set
#' @description Stock price and returns for SPY
#' @format data frame
#' @source {Yahoo Finance}
"spy"

#' Data for spot to futures convergence - historical data
#' @description Cash and futures
#' @format data frame
#' @source {Morningstar, EIA}
"spot2futConvergence"

#' Data for spot to futures convergence - forward curve
#' @description Forward Curve
#' @format data frame
#' @source {Morningstar, EIA}
"spot2futCurve"

#' Data for WTI Calendar Month Average Swap pricing
#' @description WTI Crude futures
#' @format data frame
#' @source {Morningstar}
"wtiSwap"

#' Data for USDCAD FX forward rates
#' @description USDCAD 1-year and 5-year forward points
#' @format data frame
#' @source {Morningstar}
"fxfwd"

#' Data for Eurodollar futures contracts
#' @description ED futures contract for December 2024
#' @format data frame
#' @source {Morningstar}
"eurodollar"

#' Data for IR compounding exercises
#' @description Planet metrics from NASA
#' @format data frame
#' @source <https://nssdc.gsfc.nasa.gov/planetary/factsheet/index.html>
"planets"

#' Metadata for expiry of common commodity futures contract.
#' @description This dataframe provides detailed information on major futures contracts specifications
#' pertaining to last settlement, notices and delivery dates. It also provides tickers in some data service.
#' @format data frame
"expiry_table"

#' Metadata for NYMEX and ICE holiday calendars
#' @description Holiday calendars for NYMEX and ICE Brent
#' @format data frame
"holidaysOil"

#' Metadata of key EIA tickers grouped by products.
#' @description Supports automated upload of EIA data through its API by categories.
#' Data frame organized by Supply Demand categories and products.
#' @format data frame
"tickers_eia"

#' Data for EIA weekly stocks
#' @description EIA weekly crude, NG, ULSD and RBOB stocks.
#' @format data frame
"eiaStocks"

#' Data for working storage capacity in the US
#' @description EIA working storage capacity in kbs except NG in bcf.
#' @format data frame
"eiaStorageCap"

#' Data for Canadian crude assays reported by Crude Monitor
#' @description Data set with historical Canadian Crude Assays.
#' @format data frame
#' @source <https://crudemonitor.ca/>
#' @import tsibble
"cancrudeassays"

#' Summarized data for Canadian crude assays
#' @description Data set with historical Canadian Crude Assays Statistics.
#' @format data frame
#' @source <https://crudemonitor.ca/>
#' @import tsibble
"cancrudeassayssum"

#' Data for crude assays of 50+ types of crude oil.
#' @description Crude oil qualities.
#' @format data frame
#' @source Canadian Crude Monitor and BP Crude Assays
"crudes"

#' Data for BP crude assays
#' @description Crude Assays from BP.
#' @format data frame
"crudeassaysBP"

#' Data for ExxonMobil crude assays
#' @description Crude Assays from ExxonMobil.
#' @format data frame
"crudeassaysXOM"

#' Randomized data for Canadian crude pricing.
#' @description Randomized data of Canadian Crude monthly prices versus WTI Calendar Month Average.
#' @format data frame
"cancrudeprices"

#' Data for commodity prices in a long dataframe format
#' @description Futures settlement data set.
#' @format data frame
#' @source Morningstar Commodities
"dflong"

#' Data for commodity prices in a wide dataframe format
#' @description Futures settlement data set.
#' @format data frame
#' @source Morningstar Commodities
"dfwide"

#' Data for US interest rate discounting using zero rates curve.
#' @description USD IR Discount, Forward and Zero curves from RQuantlib::DiscountCurve
#' @format List
#' #' @source Morningstar and FRED
"usSwapCurves"

#' Data for US interest rate discounting using zero rates parallel curve.
#' @description USD IR Discount, Forward and Zero curves from RQuantlib::DiscountCurve - Parallel toy data set
#' @format data frame
"usSwapCurvesPar"

#' Interest Rate Curve Data for RQuantlib .
#' @description USD IR curve input for RQuantlib::DiscountCurve
#' @format data frame
"tsQuotes"

#' Data for Canadian and US physical crude trading calendars
#' @description Crude Trading Trade Cycles
#' @format data frame
"tradeCycle"

#' GIS Data for Crude Oil Trading Hubs
#' @description Trading Hubs
#' @format data frame
"tradeHubs"

#' Randomized data of physical crude differentials
#' @description Randomized data set for education purpose of selected physical crude differentials to WTI.
#' @format data frame
"fizdiffs"

#' Metadata for teaching refinery optimization using a LP model - INPUTS
#' @description Simple refinery input to be used in running LP modeling for education purposes.
#' @format data frame
"ref.opt.inputs"

#' Metadata for teaching refinery optimization using a LP model - OUTPUTS
#' @description Simple refinery outputs and constraints to be used in running LP modeling for education purposes.
#' @format data frame
"ref.opt.outputs"

#' Data for teaching the various ways to monetize a market call.
#' @description Data set for explaining the various ways to monetize a market view.
#' @format data frame
"tradeprocess"
