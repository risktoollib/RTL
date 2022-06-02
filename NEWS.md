# RTL 1.2.0

## New

+ `simMultivariates()` generates multivariate normal random epsilons from a a historical data set.
+ `efficientFrontier()` generates Markowitz mean-variance portfolios for commodities assets i.e. risk and reward not in percentages. 
+ `simOU()` augmented with an extra parameter `epsilon` in case your simulation is part of a multivariate simulation.

## Enhancement


## Bugs & Fixes

+ UTF characters in `crudeassaysBP`.

## Remove


## Updates


# RTL 1.1.0

## New

+ `tradeHubs` contains GIS coordinates for major crude oil trading hubs in North America.
+ `tsQuotes` dataset for use with `RQuantLib::DiscountCurve()`.
+ `simOUt()` implements `simOU()` with a mean reversion level as a function of time. 

## Enhancement

+ `simGBM()` vectorized.
+ `simOU()`, `simOUt()` and `simOUJ()` implemented in `Rcpp` - see ./src/rcpp*.cpp
+ Added Mont Belvieu and TMX Burnaby to `tradeHubs`.

## Bugs & Fixes

+ Remove dependencies to `tidyquant::tq_get()`.
+ `chart_zscore()` time axis fixed.

## Remove

+ 'ir_df_us()' removed as it uses `quandl` for interest rates. Use `RTL::ir_df_us` data set instead.

## Updates

+ `expiry_table` updated and now includes LTH and HG CME contracts.
+ Removed `usSwapIR` and `getIRswapCurve()` as data is no longer available after discontinuation of LIBOR fixes..

# RTL 1.0.0

## Bugs & Fixes

+ `eia2tidy()` makes requests over `https` instead of `http` as API now requires it.

## Package Structure

+ Normalizing version control to first stable version.
+ Tidy package documentation to standards.
+ `testthat` implemented for metadata checks.

# RTL 0.1.91

+ Fixed a minor bug in `tradeCycle` dates for Canadian Crude.

# RTL 0.1.9

## Updates

+ `dfwide` retains the `NA` so as not to reduce scope where all tickers have data. 
+ `eiaStorageCap` now includes Lower 48 States Working Natural Gas Total Underground Storage Capacity. 
+ `chart_fwd_curves()` vectorized.
+ Added storage capacity to `eiaStorageCap` data set for PADD1 middle and light distillates as a proxy for NYH.
+ `tradeCycle` updated for 2022 Canadian Notice of Shipments and US Domestic crude calendar added.
+ `rolladjust()` updated with CME Canadian crude calendar cmdty == "cmecan".
+ `chart_eia_steo()` inventory imbalance subplot updated to a line fill type for better visibility. 

## Bugs & Fixes

+ Fixed `promptBeta()` period input.
+ `getPrices()` merging with all = TRUE.

## Deprecated

+ `stl_decomp()` has been removed.

# RTL 0.1.8

## Updates and Fixes

+ `tradestats` partially migrated to `tidyquant` from `quantmod`.
+ `getCurve()` updated for LME and SHFE feeds.
+ `fxfwd` dataset created for USD/CAD FX forwards.
+ `eurodollar` dataset created for eurodollar future contract.
+ `rmp` dataset created for Producer Hedging project. 
+ `dflong` and `dflong` datasets now contain CME Aluminium prices.

# RTL 0.1.7

## New

+ Added the following feeds to Morningstar API function `getPrice()`:
    + `AESO_ForecastAndActualPoolPrice`
    + Send email to pcote@ualberta.ca if you wish to add more feeds.
    
+ `getGIS()` to obtain a object from a shapefile URL. **The datasets below were removed and can be recreated as follows:**
    + crudepipelines <- `getGIS(url = "https://www.eia.gov/maps/map_data/CrudeOil_Pipelines_US_EIA.zip")`
    + refineries <- `getGIS(url = "https://www.eia.gov/maps/map_data/Petroleum_Refineries_US_EIA.zip")`
    + productspipelines <- `getGIS(url = "https://www.eia.gov/maps/map_data/PetroleumProduct_Pipelines_US_EIA.zip")`
    + productsterminals <- `getGIS(url = "https://www.eia.gov/maps/map_data/PetroleumProduct_Terminals_US_EIA.zip")`
    + ngpipelines <- `getGIS(url = "https://www.eia.gov/maps/map_data/NaturalGas_InterIntrastate_Pipelines_US_EIA.zip")`
    + ngstorage <- `getGIS(url = "https://www.eia.gov/maps/map_data/PetroleumProduct_Terminals_US_EIA.zip")`
    + nghubs <- `getGIS(url = "https://www.eia.gov/maps/map_data/NaturalGas_TradingHubs_US_EIA.zip")`
    + lngterminals <- `getGIS(url = "https://www.eia.gov/maps/map_data/Lng_ImportExportTerminals_US_EIA.zip")`

## Updates and Fixes

+ `expiry_table` updated for expiries + Yahoo Finance tickers to pull using `tidyquant::tq_get()`.
+ `eiaStorageCap`: EIA crude storage capacity by PADD.

## Removed

+ None.

# RTL 0.1.6

## New

+ Genscpae API interface added for `getGenscapeStorageOil()` and `getGenscapePipeOil()`.
+ Added the following feeds to Morningstar API function `getPrice()`:
    + `ERCOT_LmpsByResourceNodeAndElectricalBus`
    + `PJM_Rt_Hourly_Lmp`
    + Send email to pcote@ualberta.ca if you wish to add more feeds.

## Updates and Fixes

+ `chart_spreads` conversion armument now a vector allowing for different conversion e.g. crack spreads.
+ Updated `cancrudeassays` dataset. Removed AHS, WCB and SYN grades.
+ `chart_zscore()` amended. Output `stats` returns statistical tests and `res` fitted results.
+ `promptBeta()` removed output `stats`. 
+ Updated `usSwapIR`, `usSwapCurves` with rates as of `2020-12-31`.
+ `tradeCycle` table updated for Canadian crude oil 2021 calendar. Source: COLC.
+ `getPrice` fixed to return Settle instead of Open when `feed=CME_NymexOptions_EOD`.

## Removed

# RTL 0.1.5

## New

+ `chart_spreads()` to generate specific contract spreads across years e.g. ULSD March/April. Requires Morninstar credentials.

+ Morningstar feeds: 

    + CME_Comex_FuturesSettlement_EOD and CME_Comex_FuturesSettlement_EOD_continuous.
    + LME_AskBidPrices_Delayed.
    + SHFE_FuturesSettlement_RT.
    + EIA GIS data sets `ngpipelines`, `ngstorage`, `nghubs`, `lngterminals`.
    
## Updates and Fixes

+ `eia2tidy()` amended for quarterly and hourly data.
+ Morningstar tickers for `getPrice()` and `getPrices()` functions.
+ `promptBeta()` chart moved to `plotly`.

## Removed

# RTL 0.1.4

## New
+ `getCurve()` added to extract OHLC futures contract forward curves from `Morningstar`. 
+ `chart_eia_steo()` returns a Supply/Demand balance from the EIA STEO data set. Currently configured for Global Liquids and will be augmented for US Crude, Light and Middle Distillates.
+ `chart_eia_sd()` returns Supply/Demand balance from the EIA weekly data for mogas, distillates, jet and resids.
+ `tickers_eia` table updated to build Supply Demand Balances for US products.
+ Datasets `ref.opt.inputs` and `ref.opt.outputs` to support refinery LP optimization education using `lpSolve` package. 
+ `swapFutWeight()` returns the % applied to the first line contract in Calendar Month Average commodity swaps when two futures contracts are involved e.g. WTI. It uses the proper NYMEX or ICE holiday calendars and fit for purpose for building trading sheets.
+ `swapInfo()` returns all information required to price first line futures contract averaging swap or CMA physical trade, including a current month instrument with prior settlements.

## Updates and Fixes
+ `eia2tidy()` fix for key variable in function.
+ `crudes` dataset updated.

## Removed
 + `twtrump` and `twoott` tweets datasets for learning NLP.

# RTL 0.1.3

+ `eia2tidy()` removed dependency to `EIAdata` package.
+ `chart_pairs()` funtion added to render plotly pairs chart for time series.
+ Summarised statistics dataset `cancrudeassayssum ` for Canadian Crude assays.
+ Crude oil qualities dataset `crudes ` from crudemonitor.ca and BP Assays.
+ Sample GIS data sets for North American `refineries` and `crudepipelines`.
+ Added `eiaStocks` and `eiaStorageCap` data sets.
+ Upgrade for `dplyr 1.0.0`.
+ Sample data sets `dflong` and `dfwide` updated.
+ Added `crudeassaysXOM` as a list for complete public assays from ExxonMobil.
+ Added `planets` data for interest rate exercises.


# RTL 0.1.1

+ RTL rebuilt to CRAN standards and published.

