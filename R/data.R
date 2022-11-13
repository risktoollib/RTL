#' dataset: Cushing c1c2 and storage utilization
#' @description Sample dataset for time spreads vs storage utilization.
#' @format data frame
"cushingStorage"

#' dataset: futures contract codes
#' @description Exchange-traded contract month codes.
#' @format data frame
"futuresMonths"

#' dataset: CL contract specifications
#' @description Sample futures specifications for NYMEX crude oil.
#' @format data frame
"futuresSpecs"

#' dataset: EIA Short Term Energy Outlook
#' @description Short Term Energy Outlook from the EIA.
#' @format plotly object
#' @source {eia}
"steo"

#' dataset: front line WTI contract OHLC
#' @description Historical CL 1st Contract OHLC set for algo trading example.
#' @format data frame
#' @source {CME}
"CLc1"

#' dataset: second line WTI contract OHLC
#' @description Historical CL 2nd Contract Close adjusted using ohlc object.
#' @format data frame
#' @source {CME}
"CLc2"

#' dataset: WTI c1c2 OHLC
#' @description Historical CL 1st vs 2nd Contract spread.
#' @format data frame
#' @source {CME}
"CLc1c2"

#' dataset: randomiser to convert settlement into OHLC
#' @description OHLC profile using historical CL 1st Contract OHLC
#' @format data frame
#' @source {CME}
"ohlc"

#' dataset: SPY ETF
#' @description Stock price and returns for SPY
#' @format data frame
#' @source {Yahoo Finance}
"spy"

#' dataset: USO ETF
#' @description Stock price and returns for US Crude Oil ETF
#' @format data frame
#' @source {Yahoo Finance}
"uso"

#' dataset: RY share price
#' @description Adjusted RBC stock price on NYSE with dividend yield.
#' @format data frame
#' @source {Yahoo Finance}
"ry"

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

#' dataset: Canadian crude assays reported by Crude Monitor
#' @description Data set with historical Canadian Crude Assays.
#' @format data frame
#' @source <https://beta.crudemonitor.ca/>
#' @import tsibble
"cancrudeassays"

#' dataset: Crude assays of 50+ types of crude oil
#' @description Crude oil qualities.
#' @format data frame
#' @source Canadian Crude Monitor and BP Crude Assays
"crudes"

#' dataset: BP crude assays
#' @description data: Crude Assays from BP.
#' @format data frame
"crudeassaysBP"

#' dataset: ExxonMobil crude assays
#' @description Crude Assays from ExxonMobil.
#' @format data frame
"crudeassaysXOM"

#' dataset: randomised Canadian crude pricing.
#' @description Randomized data of Canadian Crude monthly prices versus WTI Calendar Month Average.
#' @format data frame
"cancrudeprices"

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

#' dataset: refinery optimization using a LP model - inputs
#' @description Simple refinery input to be used in running LP modeling for education purposes.
#' @format data frame
"ref.opt.inputs"

#' dataset: refinery optimization using a LP model - outputs
#' @description Simple refinery outputs and constraints to be used in running LP modeling for education purposes.
#' @format data frame
"ref.opt.outputs"

#' dataset: data for teaching the various ways to monetize a market call.
#' @description Data set for explaining the various ways to monetize a market view.
#' @format data frame
"tradeprocess"
