#' planets
#' @description Planet metrics from NASA
#' @format data frame
#' @source \url{https://nssdc.gsfc.nasa.gov/planetary/factsheet/index.html}
"planets"

#' expiry_table
#' @description This dataframe provides detailed information on major futures contracts specifications
#' pertaining to last settlement, notices and delivery dates. It also provides tickers in some data service.
#' @format data frame
"expiry_table"

#' holidaysOil
#' @description Holiday calendars for NYMEX and ICE Brent
#' @format data frame
"holidaysOil"

#' tickers_eia
#' @description Supports automated upload of EIA data through its API by categories.
#' Data frame organized by Supply Demand categories and products.
#' @format data frame
"tickers_eia"

#' eiaStocks
#' @description EIA weekly crude, NG, ULSD and RBOB stocks.
#' @format data frame
"eiaStocks"

#' eiaStorageCap
#' @description EIA crude storage capacity in thousand bbls.
#' @format data frame
"eiaStorageCap"

#' cancrudeassays
#' @description Data set with historical Canadian Crude Assays.
#' @format data frame
#' @source \url{https://crudemonitor.ca/}
"cancrudeassays"

#' cancrudeassayssum
#' @description Data set with historical Canadian Crude Assays Statistics.
#' @format data frame
#' @source \url{https://crudemonitor.ca/}
"cancrudeassayssum"

#' crudes
#' @description Crude oil qualities.
#' @format data frame
#' @source Canadian Crude Monitor and BP Crude Assays
"crudes"

#' crudeassaysBP
#' @description Crude Assays from BP.
#' @format data frame
#' @source \url{https://www.bp.com/en/global/bp-global-energy-trading/features-and-updates/technical-downloads/crudes-assays.html}
"crudeassaysBP"

#' crudeassaysXOM
#' @description Crude Assays from ExxonMobil.
#' @format data frame
#' @source \url{https://corporate.exxonmobil.com/Crude-oils/Crude-trading/Crude-oil-blends-by-API-gravity-and-by-sulfur-content#APIgravity}
"crudeassaysXOM"

#' cancrudeprices
#' @description Randomized dataset of Canadian Crude monthly prices versus WTi Calendar Month Average.
#' @format data frame
"cancrudeprices"

#' df_fut
#' @description Futures settlement data set.
#' @format data frame
#' #' @source \url{https://www.morningstar.com/products/commodities-and-energy}
"df_fut"

#' dflong
#' @description Futures settlement data set.
#' @format data frame
#' #' @source \url{https://www.morningstar.com/products/commodities-and-energy}
"dflong"

#' dfwide
#' @description Futures settlement data set.
#' @format data frame
#' #' @source \url{https://www.morningstar.com/products/commodities-and-energy}
"dfwide"

#' usSwapIR
#' @description USD Interest Rate Swap Curve for RQuantlib bootstrapping. See usSwapIRdef for sources and tickers.
#' @format data frame
#' #' @source Morningstar and FRED
"usSwapIR"

#' usSwapIRdef
#' @description USD Interest Rate Swap Curve definitions with sources and tickers
#' @format data frame
#' #' @source Morningstar and FRED
"usSwapIRdef"

#' usSwapCurves
#' @description USD IR Discount, Forward and Zero curves from RQuantlib::DiscountCurve
#' @format List
#' #' @source Morningstar and FRED
"usSwapCurves"

#' usSwapCurvesPar
#' @description USD IR Discount, Forward and Zero curves from RQuantlib::DiscountCurve - Parallel toy data set
#' @format data frame
"usSwapCurvesPar"

#' tradeCycle
#' @description Crude Trading Trade Cycles
#' @format data frame
"tradeCycle"

#' fizdiffs
#' @description Randomized data set for education purpose of selected physical crude differentials to WTI.
#' @format data frame
"fizdiffs"

#' ref.opt.inputs
#' @description Simple refinery input to be used in running LP modeling for education purposes.
#' @format data frame
"ref.opt.inputs"

#' ref.opt.ouputs
#' @description Simple refinery outputs and constraints to be used in running LP modeling for education purposes.
#' @format data frame
"ref.opt.outputs"

#' tradeprocess
#' @description Data set for explaning the various ways to monetize a market view.
#' @format data frame
"tradeprocess"
