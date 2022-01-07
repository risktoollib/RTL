library(testthat)
library(RTL)

#test_check("RTL")

test_that("tradeCycle Canadian",{
  x = tradeCycle %>% dplyr::mutate(diff = as.numeric(.[[3]]-.[[2]]))
  expect_lt(x %>% dplyr::filter(market == "canada") %>% dplyr::select(diff) %>% min(.),-10)
  expect_gt(x %>% dplyr::filter(market == "canada") %>% dplyr::select(diff) %>% min(.),-20)
})

test_that("tradeCycle US Domestic",{
  x = tradeCycle %>% dplyr::mutate(diff = as.numeric(.[[3]]-.[[2]]))
  expect_lt(x %>% dplyr::filter(market == "usdomestic") %>% dplyr::select(diff) %>% min(.),-5)
  expect_gt(x %>% dplyr::filter(market == "usdomestic") %>% dplyr::select(diff) %>% min(.),-15)
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
