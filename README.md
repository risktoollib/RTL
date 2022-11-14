
<!-- README.md is generated from README.Rmd. Please edit that file -->
<!-- badges: start -->

[![CRAN_Status_Badge](https://www.r-pkg.org/badges/version/RTL)](https://cran.r-project.org/package=RTL)
[![metacran
downloads](https://cranlogs.r-pkg.org/badges/RTL)](https://cran.r-project.org/package=RTL)
[![R-CMD-check](https://github.com/risktoollib/RTL/workflows/R-CMD-check/badge.svg)](https://github.com/risktoollib/RTL/actions)
<!-- badges: end -->

## Overview

RTL is a purposely designed for traders, analysts and risk practitioners
in Commodities and Finance. It also supports delivery of Finance classes
from one of the creator also in Academics at the [Alberta School of
Business](https://www.ualberta.ca/business/index.html).

Send feedback to `pcote@ualberta.ca`. We welcome feedback, suggestions
and collaboration.

### Energy Markets Reference Data

- `expiry_table`: NYMEX and ICE contracts expiry tables.
- `holidaysOil`: Holiday calendars for NYMEX and ICE.
- `tradeCycle`: US and Canadian crude oil trading calendars.
- `tickers_eia`: Mapping of EIA tickers to crude and refined products
  markets for building supply demand balances.
- `eiaStorageCap`: Historical EIA crude storage capacity by PADD.
- `eiaStocks`: Sample data set of EIA.gov stocks for key commodities.

### Swap Pricing and Contract Roll Adjustement

- `rolladjust()` adjusts continuous contracts returns for roll
  adjustments using `expiry_table`.
- `swapCOM()` computes Calendar Month Average commodity swap prices.
- `swapInfo()` returns all information required to price first line
  futures contract averaging swap or CMA physical trade, including a
  current month instrument with prior settlements.
- `swapIRS()` computes IRS swap prices.

### Charting

- `chart_fwd_curves()`: plots historical forward curves, a useful
  feature to understand the pricing dynamics of a market.
- `chart_zscore()` supports seasonality adjusted analysis of residuals,
  particularly useful when dealing with commodity stocks and/or days
  demand time series with trends as well as non-constant variance across
  seasonal periods.
- `chart_eia_steo()` and `chart_eia_sd()` return either a chart or
  dataframe of supply demand balances from the EIA.
- `chart_spreads()` to generate specific contract spreads across years
  e.g. ULSD March/April. Requires Morninstar credentials.
- …

### APIs

Valid credentials for commercial API services are required.

#### Genscape

[Genscape API](https://developer.genscape.com/) functions:

- `getGenscapeStorageOil()`.
- `getGenscapePipeOil()`.

#### Morningstar Commodities

[Morningstar Marketplace
API](https://mp.morningstarcommodity.com/marketplace/) functions:

- `getPrice()`, `getPrices()` and `getCurve()` using your own
  Morningstar credentials. Current feeds included:

  - ICE_EuroFutures and ICE_EuroFutures_continuous.
  - CME_NymexFutures_EOD and CME_NymexFutures_EOD_continuous.
  - CME_NymexOptions_EOD.
  - CME_CbotFuturesEOD and CME_CbotFuturesEOD_continuous.
  - CME_Comex_FuturesSettlement_EOD and
    CME_Comex_FuturesSettlement_EOD_continuous.
  - LME_AskBidPrices_Delayed.
  - CME_CmeFutures_EOD and CME_CmeFutures_EOD_continuous.
  - CME_STLCPC_Futures.
  - ICE_NybotCoffeeSugarCocoaFutures and
    ICE_NybotCoffeeSugarCocoaFutures_continuous.
  - Morningstar_FX_Forwards.
  - … see `?getPrice` for up to date selection and examples.

### GIS Dataset

#### EIA

- crudepipelines \<-
  `getGIS(url = "https://www.eia.gov/maps/map_data/CrudeOil_Pipelines_US_EIA.zip")`
- refineries \<-
  `getGIS(url = "https://www.eia.gov/maps/map_data/Petroleum_Refineries_US_EIA.zip")`
- productspipelines \<-
  `getGIS(url = "https://www.eia.gov/maps/map_data/PetroleumProduct_Pipelines_US_EIA.zip")`
- productsterminals \<-
  `getGIS(url = "https://www.eia.gov/maps/map_data/PetroleumProduct_Terminals_US_EIA.zip")`
- ngpipelines \<-
  `getGIS(url = "https://www.eia.gov/maps/map_data/NaturalGas_InterIntrastate_Pipelines_US_EIA.zip")`
- ngstorage \<-
  `getGIS(url = "https://www.eia.gov/maps/map_data/PetroleumProduct_Terminals_US_EIA.zip")`
- nghubs \<-
  `getGIS(url = "https://www.eia.gov/maps/map_data/NaturalGas_TradingHubs_US_EIA.zip")`
- lngterminals \<-
  `getGIS(url = "https://www.eia.gov/maps/map_data/Lng_ImportExportTerminals_US_EIA.zip")`
- `tradeHubs` has North Americas crude oil trading hubs (WIP).

#### Alberta Oil Sands, Petroleum and Natural Gas

- AB \<-
  `getGIS(url = "https://gis.energy.gov.ab.ca/GeoviewData/OS_Agreements_Shape.zip")`

### Interest Rates

- `usSwapIR`: Sample data set output of `getIRswapCurve`.
- `usSwapCurves`: Sample data set output of
  `RQuantlib::DiscountCurve()`.

## Python

A python version of RTL for most functions is available at
<https://pypi.org/project/risktools/>.

## Installation

**Latest Package** `devtools::install_github("risktoollib/RTL")`

**CRAN Stable** `install.packages("RTL")`
