library(testthat)
library(dplyr)
library(tidyr)
library(RTL)

#test_check("RTL")

test_that("tradeCycle Canadian",{
  x = tradeCycle %>% dplyr::mutate(diff = as.numeric(trade.cycle.end-.[[2]]))
  expect_lt(x %>% dplyr::filter(market == "canada") %>% dplyr::select(diff) %>% min(.),-10)
  expect_gt(x %>% dplyr::filter(market == "canada") %>% dplyr::select(diff) %>% min(.),-21)
})

test_that("tradeCycle US Domestic",{
  x = tradeCycle %>% dplyr::mutate(diff = as.numeric(trade.cycle.end-.[[2]]))
  expect_lt(x %>% dplyr::filter(market == "usdomestic") %>% dplyr::select(diff) %>% min(.),-5)
  expect_gt(x %>% dplyr::filter(market == "usdomestic") %>% dplyr::select(diff) %>% min(.),-15)
})


test_that("barrierSpreadOption Boundary Conditions", {
  # Test setup
  epsilon <- 1e-10
  F1 <- -12.00
  F2 <- -5.00
  T2M <- epsilon
  sigma1 <- 0.4
  sigma2 <- 0.4
  rho <- 0.8
  r <- 0.045

  results <- tidyr::expand_grid(  # Using expand_grid instead of expand.grid for tibble output
    f1 = seq(-15, -8, by = 0.1),
    F2 = F2,
    monitoring = c("continuous", "terminal"),
    type = c("call","put")
  ) %>%
    dplyr::mutate(
      spread = F2 - f1,
      X = ifelse(type == "call", 5, 7),
      B = ifelse(type == "call", 9, 5),
      barrier_type = ifelse(type == "call", "uo", "do"),
      intrisic = dplyr::case_when(
        type == "call" ~ ifelse(spread < B, pmax(0, spread - X), 0),
        type == "put" ~ ifelse(spread > B, pmax(0, X - spread), 0)
      ),
      price = 0
    )

  # Calculate prices row by row
  for(i in 1:nrow(results)) {
    res <- RTL::barrierSpreadOption(
      F1 = results$f1[i],
      F2 = results$F2[i],
      X = results$X[i],
      B = results$B[i],
      sigma1 = sigma1,
      sigma2 = sigma2,
      rho = rho,
      T2M = T2M,
      r = r,
      type = results$type[i],
      barrier_type = results$barrier_type[i],
      monitoring = results$monitoring[i]
    )
    results$price[i] <- res$price
  }

  # Test that all prices match intrinsic values at expiry
  all_match <- all(dplyr::near(results$price, results$intrisic, tol = epsilon))
  expect_true(all_match,
              label = "All prices should match intrinsic values at expiry")

  # Up and out options should have zero prices above barrier
  zz = results %>% dplyr::filter(barrier_type == "uo", spread >= B)
  expect_true(
    all(dplyr::near(zz$price, zz$intrisic, tol = epsilon)),
    label = "Prices should be zero above barrier for up-and-out options"
  )

  # Down and out options should have zero prices above barrier
  zz = results %>% dplyr::filter(barrier_type == "do", spread <= B)
  expect_true(
    all(dplyr::near(zz$price, zz$intrisic, tol = epsilon)),
    label = "Prices should be zero below barrier for dowm-and-out options"
  )

  # All prices must be non-negative
  expect_true(
    all(results$price >= 0),
    label = "All prices should be non-negative"
  )

})



#
# dataGaps = dfwide %>%
#   tidyr::pivot_longer(-date,"series","value")%>% dplyr::group_by(series) %>%
#   dplyr::mutate(diff = dplyr::lag(date),
#                 diff = date - diff) %>%
#   tidyr::drop_na() %>%
#   dplyr::summarise(missingDays = max(diff)) %>%
#   dplyr::filter(missingDays > 5)
#
# ss <- dataGaps$series[24]
#
# dfwide %>%
#   dplyr::select(date,ss) %>%
#   filter_all(any_vars(is.na(.)))
#   dplyr::filter(!is.na(ss))
#
#   dataGaps <-  dfwide %>%
#     dplyr::select(date,BRN01) %>%
#     dplyr::mutate(dd = as.numeric(date),
#                   diff = dplyr::lag(dd),
#                   diff2 = diff - dd)
#
