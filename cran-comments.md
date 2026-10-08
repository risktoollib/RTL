## R CMD check results

0 errors | 0 warnings | 0 notes

## Test environments

* local macOS 27.2 (aarch64-apple-darwin23), R 4.6.1, `R CMD check --as-cran`

## Response to CRAN comments

> Imports includes 21 non-default packages. Importing from so many packages makes the package vulnerable to any of them becoming unavailable. Move as many as possible to Suggests and use conditionally.

Imports are reduced from 21 to 11 packages:

* `numDeriv`, `glue` and `lifecycle` were unused and are removed.
* `rlang`, `magrittr`, `tidyselect` and `tibble` are no longer imported; the few symbols used (`%>%`, `.data`, `tibble()`, `tribble()`, `where()`) come from `dplyr`'s re-exports.
* `ggplot2`, `PerformanceAnalytics` and `tsibble` move to Suggests. The functions that use them (`chart_PerfSummary()`, `chart_zscore()`, `promptBeta()`) check for them with `requireNamespace()` and stop with an install message, and the one example that runs is wrapped in `@examplesIf`.

The remaining Imports (`dplyr`, `tidyr`, `lubridate`, `xts`, `zoo`, `purrr`, `stringr`, `plotly`, `httr`, `jsonlite`, `Rcpp`) are used by the package's core pricing, data and charting functions.
