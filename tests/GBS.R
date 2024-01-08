library(tidyverse)
S = 100 # stock price at T = 0
X = 100 # strike price
T2M = 1 # maturity in years
# all rates are continuous compounding
r = 0.1 # interest rate
d = 0 # dividend yield
b = r - d # cost of carry yield
sigma = 0.25 # implied volatility
n = 500 # number of simulations where required.

AmerEur <- expand.grid(r = seq(0.01,0.05,.01), d = seq(0.01,0.5,.01))

res <- AmerEur %>%
  dplyr::mutate(Eur = 0, Amer = 0,S = S, X = X, T2M = T2M, b = r-d, sigma = sigma, N = n)  %>%
  dplyr::mutate(Eur = purrr::pmap(.l = list(type = "call", optionStyle = "european",S = S,X = X,T2M = T2M,r = r,b = b,sigma = sigma,N = N),
                                  .f = RTL::CRROption)) %>%
  dplyr::mutate(Eur = purrr::map(Eur,.f = function(x) x$price)) %>%
  unnest(Eur) %>%
  dplyr::mutate(Amer = purrr::pmap(.l = list(type = "call", optionStyle = "american",S = S,X = X,T2M = T2M,r = r,b = b,sigma = sigma,N = N),
                                   .f = RTL::CRROption)) %>%
  dplyr::mutate(Amer = purrr::map(Amer,.f = function(x) x$price)) %>%
  unnest(Amer) %>%
  dplyr::mutate(EurFopt = 0, AmerFopt = 0,S = S, X = X, Time = T2M, b = r-d, sigma = sigma, n = n)  %>%
  dplyr::mutate(EurFopt = purrr::pmap(.l = list(TypeFlag = "ce",S,X,Time,r,b,sigma,n),
                                  .f = fOptions::CRRBinomialTreeOption)) %>%
  dplyr::mutate(EurFopt = purrr::map(EurFopt,.f = function(x) x@price)) %>%
  unnest(EurFopt) %>%
  dplyr::mutate(AmerFopt = purrr::pmap(list(TypeFlag = "ca",S,X,Time,r,b,sigma,n),
                                   .f = fOptions::CRRBinomialTreeOption)) %>%
  dplyr::mutate(AmerFopt = purrr::map(AmerFopt,.f = function(x) x@price)) %>%
  unnest(AmerFopt) %>%
  dplyr::mutate(EurDiff = Eur - EurFopt,
                AmerFoptDiff = Amer - AmerFopt)

d = .01
r = .01
RTL::CRROption(S = 100, X = 100, sigma = 0.25, r = r, b = r-d, T2M = 1, N = 500, type = "call", optionStyle = "european")$price
fOptions::CRRBinomialTreeOption(TypeFlag = "ce",S = 100, X = 100, Time = 1, r = r, b = r-d, sigma = 0.25, n = 500)@price
