# RTL 1.4.1

## Breaking changes

* `simOUJ()` now simulates the mean-reverting jump-diffusion of Clewlow & Strickland (2000), eq. 2.17: dS = theta (ln mu - ln S) S dt + sigma S dz + kappa S dq, stepped in ln S (eq. 2.3), so S stays positive. The arguments keep their names but change meaning:
  * `mu` is the long-term level of S (Clewlow & Strickland's e^mu), as before a price level.
  * `theta` is the reversion speed of ln S.
  * `sigma` is a proportional volatility (0.2 = 20% a year); it was an absolute one.
  * `jump_avesize` is the mean proportional jump, kappa bar (0.4 = +40%); it was a jump size in price units. The default moves from 2 to 0.4.
  * `jump_stdv` is the jump volatility, the standard deviation of ln(1 + kappa) (eq. 2.15).

  The old process was arithmetic, subtracted `jump_prob * jump_avesize` from the reversion level, and drew a single jump size for every jump in a call (`rlnorm(n = 1)`). With frequent jumps its reversion level went negative: at `jump_prob = 4` and `jump_avesize = 2` it reverted to 5 - 8 = -3, and 5% of paths ended below -2.3 after a year. Each jump now draws its own size.

## Bugs & Fixes

* `getPrice()` ERCOT timestamps use `America/Chicago`. They used `"CST"`, which is not a valid time zone, so R warned and fell back to UTC.

## Dependencies

* Imports cut from 21 to 11 packages. `ggplot2`, `PerformanceAnalytics` and `tsibble` move to Suggests; `chart_PerfSummary()`, `chart_zscore()` and `promptBeta()` ask for them when needed. `numDeriv`, `glue`, `lifecycle`, `rlang`, `magrittr`, `tidyselect` and `tibble` are no longer imported.

# RTL 1.4.0

## Bugs & Fixes

* `spreadOption()` now implements Kirk (1995): the spread volatility weights only `sigma1`, by `F1 / (F1 + X)`. It used `F1 / (F1 + F2)` and `F2 / (F1 + F2)` on both legs, which understated the volatility and underpriced calls by 34% and 50% on two test cases checked against a 2,000,000-path Monte Carlo.
* `spreadOption()` Greeks are now the exact derivatives of the price. `theta` had the wrong sign and lost its time-decay term, `rho` had the wrong sign, `gamma_cross` divided by `F1 + X` instead of `F2`, and `delta_F1`, `gamma_F1`, `vega_1` and `vega_2` ignored that the Kirk volatility depends on `F1` and used the wrong weights. Tests compare every Greek with a finite difference of the price.
* `GBSOption()` theta had the sign of its cost-of-carry term flipped, `+ (b - r) S e^{(b-r)T} N(d1)` for a call instead of `- (b - r) ...` (and the reverse for a put), so theta was wrong whenever `b != r`, including Black (1976) on futures (`b = 0`).
* `GBSOption()` rho is now `-T2M * price` for `b = 0` (an option on a futures price, where the cost of carry stays zero as the rate moves); for `b != 0` it keeps `q = r - b` fixed, as before. Tests compare every Greek with a finite difference of the price.
* `spreadOption()` stops with an error outside Kirk's approximation (`F2 <= 0`, `F1 + X <= 0`, `T2M <= 0`, `|rho| > 1`, zero spread volatility) instead of returning a number.

# RTL 1.3.9

## Enhancement


## Bugs & Fixes


# RTL 1.3.7 and before

Deleted and history reset to v 1.3.8
