library(testthat)
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


# test_that("barrier spread options",{
#   # Test setup
#   library(tidyverse)
#
#   F1 <- -12.00
#   F2 <- -5.00
#   X <- 4.00
#   B <- 9.5
#   sigma1 <- 0.6
#   sigma2 <- .6
#   rho <- 0.3
#   r <- 0.01
#
#   # Create a data frame with all combinations
#   F1_values <- seq(-15, -8, by = 0.01)
#   results <- expand.grid(
#     f1 = F1_values,
#     T2M = c(1/24)
#   ) %>%
#     as_tibble() %>%
#     mutate(
#       spread = F2 - f1,
#       Delta1 = NA_real_,
#       Delta2 = NA_real_
#     )
#
#   # Calculate deltas row by row
#   for(i in 1:nrow(results)) {
#     res <- RTL::barrierSpreadOption(
#       F1 = results$f1[i],
#       F2 = F2,
#       X = X,
#       B = B,
#       sigma1 = sigma1,
#       sigma2 = sigma2,
#       rho = rho,
#       T2M = results$T2M[i],
#       r = r,
#       type = "call",
#       barrier_type = "uo",
#       monitoring = "continuous"
#     )
#     results$Delta1[i] <- res$delta_F1
#     results$Delta2[i] <- res$delta_F2
#   }
#
#   # Plot
#   ggplot(results, aes(x = spread)) +
#     geom_hline(yintercept = c(-1, 0, 1), linetype = "dashed", color = "gray") +
#     geom_vline(xintercept = c(X, B), linetype = "dashed", color = "red", alpha = 0.5) +
#     geom_line(aes(y = Delta1, color = "Delta 1", linetype = factor(T2M)), linewidth = 1) +
#     geom_line(aes(y = Delta2, color = "Delta 2", linetype = factor(T2M)), linewidth = 1) +
#     scale_color_manual(values = c("Delta 1" = "blue", "Delta 2" = "red")) +
#     scale_linetype_manual(
#       values = c("solid", "dashed"),
#       labels = c("1 day", "6 months")
#     ) +
#     coord_cartesian(ylim = c(-2, 2)) +
#     labs(
#       title = "Deltas for Up-and-Out Call Spread Option",
#       subtitle = "Comparison of Different Time to Maturities",
#       x = "Spread Level",
#       y = "Delta",
#       color = "Greeks",
#       linetype = "Time to Maturity"
#     ) +
#     theme_minimal()
#
#   expect_equal(1,1)
# })



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
