#' Multivariate normal from historical dataset
#' @description Generates multivariate random epsilons using absolute returns.
#' @param nsims Number of simulations. Defaults to 10. `numeric`
#' @param x Wide data frame of prices with date as first column. `tibble`
#' @param s0 Vector of starting value for each variables. Defaults to NULL with zero. `numeric`
#' @returns List of means, sds, covariance matrix, correlation matrix and simulated values. `list`
#' @export simMultivariates
#' @author Philippe Cote
#' @examples
#' simMultivariates(nsims = 10, x = RTL::fizdiffs, s0 = NULL)
simMultivariates <- function(nsims = 10, x, s0 = NULL) {
  if (!requireNamespace("MASS", quietly = TRUE)) {stop("Package \"tidyquant\" needed for this function to work. Please install it.", call. = FALSE)}
  ret <- out <- sds <- corMat <- coVaR <- mus <- series <- NULL
  ret <- x %>%
    tidyr::pivot_longer(-date,names_to = "series", values_to = "value") %>%
    dplyr::group_by(series) %>%
    dplyr::mutate(ret = value - dplyr::lag(value)) %>%
    tidyr::drop_na() %>%
    dplyr::select(-value) %>%
    tidyr::pivot_wider(names_from = series,values_from = ret)

  aves <- ret %>% dplyr::summarise_if(is.numeric, mean)
  sds <- ret %>% dplyr::summarise_if(is.numeric, stats::sd)
  corMat <- stats::cor(ret[,-1], method = "kendall")
  coVaR = diag(sds) %*% corMat %*% diag(sds)
  if (is.null(s0)) {s0 <- rep(0,times = length(sds))} else {s0 <- x[,-1] %>% dplyr::slice_tail() %>% as.numeric(.)}
  sims <- MASS::mvrnorm(n = nsims, mu = s0, Sigma = coVaR) %>%
    dplyr::as_tibble(., .name_repair = "minimal")
  names(sims) <-  names(x)[-1]

  coVaR <- coVaR %>% dplyr::as_tibble(.name_repair = "minimal")
  names(coVaR) <-  names(x)[-1]
  coVaR <- coVaR %>%
    dplyr::mutate(series = names(x)[-1]) %>%
    dplyr::select(series,dplyr::everything())

  out <- list(
    "ave" = aves,
    "sd" = sds,
    "corMat" = corMat,
    "coVaR" = coVaR,
    "sims" = sims
  )
  return(out)
}
