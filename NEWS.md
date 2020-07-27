# RTL 0.1.4

## New
+ `getCurve()` added to extract OHLC futures contract forward curves from `Morningstar`. 
+ `chart_eia_steo()` returns a Supply/Demand balance from the EIA STEO data set. Currently configured for Global Liquids and will be augmented for US Crude, Light and Middle Distillates.

## Updates and Fixes
+ `eia2tidy()` fix for key variable in function.
+ `crudes` dataset updated.

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

# RTL 0.1.2

+ `expiry_table` fixed for 2021-2024 CME WTI Futures.
+ Added Morningstar `CME_NymexOptions_EOD` feed.

# RTL 0.1.1

+ RTL rebuilt to CRAN standards and published.

