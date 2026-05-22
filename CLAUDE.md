# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Package Overview

**RTL** (Risk Tool Library) is a CRAN R package for commodities analytics, risk management, and trading. It combines exported R functions, Rcpp-compiled C++ code, and a suite of reference datasets. It supports academic delivery at the Alberta School of Business and is mirrored as the `risktools` Python package on PyPI.

## Common Commands

All commands run from an R session in the project root.

```r
# Document (regenerates NAMESPACE and man/ from roxygen2 comments)
devtools::document()

# Build and check (standard)
devtools::check()

# Build and check with full CRAN-equivalent checks (use before any CRAN submission)
# Adds --as-cran flag which enables namespace-reference checks, URL checks, etc.
devtools::check(cran = TRUE)

# Run all tests
devtools::test()

# Run the single test file directly
source("tests/testthat/testthat.R")

# Install package locally
devtools::install()

# Rebuild a specific dataset (run from data-raw/)
setwd("data-raw"); source("DATASET.R")  # or source just the relevant block
```

## Architecture

### R Functions (`R/`)
Functions fall into these domains:
- **Swap pricing**: `swapCOM`, `swapFutWeight`, `swapInfo`, `swapIRS`, `rolladjust`
- **Options/derivatives**: `GBSOption`, `CRROption`, `CRReuro`, `barrierSpreadOption`, `spreadOption`, `bond`
- **Simulations**: `simGBM`, `simOU`, `simOUJ`, `simOUt`, `simMultivariates`, `fitOU`
- **Charting**: `chart_fwd_curves`, `chart_zscore`, `chart_spreads`, `chart_eia_sd`, `chart_eia_steo`, `chart_pairs`, `chart_PerfSummary`
- **APIs**: `zema`, `genscape`/`getGenscape*`, `getBoC`, `getGIS`, `eia2tidy`, `eia2tidy_all`
- **Quant trading**: `tradeStrategySMA`, `tradeStrategyDY`, `tradeStats`, `promptBeta`, `efficientFrontier`
- **Utilities**: `returns`, `npv`, `refineryLP`, `garch`

### Compiled C++ (`src/`)
Rcpp is used for performance-critical calculations: `CRROptionCpp.cpp`, `gbs.cpp`, `rcppOU.cpp`, `rcppOUJ.cpp`, `rcppOUt.cpp`. Compiled output (`.so`, `.o`) is in `src/` — do not edit compiled artifacts.

### Datasets (`data/`, `data-raw/`)
- `data/` — binary `.rda` files loaded lazily by the package
- `data-raw/DATASET.R` — the single 1100-line script that builds all datasets; each dataset has its own clearly delimited block
- `data-raw/*.feather` — source files for large datasets (`dflong`, `dfwide`, `wtiSwap`, `fizdiffs`); read with `arrow::read_feather()` then `dplyr::as_tibble()` before saving — `options(arrow.use_altrep = FALSE)` must be set first to prevent Arrow ALTREP-backed column types (`arrow::array_dbl_vector`, etc.) from being serialized into the `.rda` file with arrow namespace references. `as.data.frame()` in the pipe is insufficient; only disabling ALTREP at the source prevents it.
- `data-raw/tradeCycle.csv` — Canadian crude trading calendar source; Canadian rows sourced from the COLC Forecasting Calendar (colcomm.com); US domestic rows derived from `expiry_table` inside DATASET.R
- `data-raw/holidays.csv` — two-column CSV (`nymex`, `ice`) of exchange holiday dates; read with `tidyr::gather()` into long format as `holidaysOil`

### Key Reference Datasets
- `expiry_table` — futures contract expiry dates by commodity (`cmdty` column)
- `holidaysOil` — long-format tibble with `key` (`"nymex"` or `"ice"`) and `value` (Date); used for business day calendars via `bizdays`
- `tradeCycle` — Canadian and US domestic crude trading calendars with business-day-weighted pricing vectors (`bizDays`, `pricedIn` list columns)
- `tickers_eia` — EIA API ticker mapping

## Data Pipeline Pattern

When updating a dataset:
1. Edit the source file (`data-raw/*.csv`, `data-raw/*.feather`, or the inline code in `DATASET.R`)
2. Source the relevant block of `data-raw/DATASET.R` from within `data-raw/` (set working directory first)
3. The block ends with `usethis::use_data(<object>, overwrite = TRUE)` which writes to `data/`
4. `holidaysOil` must be rebuilt before `tradeCycle` since `tradeCycle` uses it for NYMEX business day counting
5. Reset working directory back to project root afterward: `setwd("..")` — subsequent `devtools::*` and `spelling::*` calls require the project root
6. After rebuilding any feather-backed dataset, verify no Arrow namespace is embedded — output must be empty:
   ```bash
   for f in dflong dfwide fizdiffs wtiSwap; do
     echo -n "$f: "; bzcat data/${f}.rda | strings | grep -i "arrow" | head -1 || echo "clean"
   done
   ```

## CRAN Submission Checklist

Before submitting to CRAN (via `devtools::check()` then win-builder):
- Run `devtools::check(cran = TRUE)` locally with no ERRORs or WARNINGs — `cran = TRUE` is required to enable `--as-cran` and catch the same namespace, URL, and policy checks that win-builder runs
- Verify feather-backed `.rda` files contain no Arrow ALTREP references (all output must be empty): `for f in dflong dfwide fizdiffs wtiSwap; do echo -n "$f: "; bzcat data/${f}.rda | strings | grep -i "arrow" | head -1 || echo "clean"; done`
- Confirm `arrow` does not appear in DESCRIPTION — it is a `data-raw`-only dependency and must not leak into the package
- Bump version in DESCRIPTION and update `Date:`
- Update `cran-comments.md` with test environments and any notes

## Coding Conventions

- All R code uses **tidyverse style** with explicit `package::function()` notation throughout
- The magrittr `%>%` pipe is used (not the native `|>`)
- Packages used only in specific non-core functions (`TTR`, `timetk`) are in `Suggests` with `requireNamespace()` guards at the top of those functions
- Data files read from `arrow::read_feather()` require `options(arrow.use_altrep = FALSE)` to be set **before** the read call. Neither `as.data.frame()` nor `dplyr::as_tibble()` strip ALTREP — they preserve Arrow ALTREP-backed column types (`arrow::array_dbl_vector`, `arrow::array_string_vector`) whose class definitions live in the arrow namespace and are serialized into the `.rda` file, causing `R CMD check` (r-devel) to warn about undeclared namespace dependencies.

## `.Rbuildignore` Entries
`.github`, `.claude`, `CRAN-SUBMISSION`, `LICENSE.md`, `README.Rmd`, `RTL.Rproj`, `data-raw`, `.Rproj.user`, `.positai` are all excluded from the package tarball.
