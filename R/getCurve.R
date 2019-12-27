#' \code{getCurve}
#' @description Computes Calendar Month Averages for WTI and Brent and IV for forward starting swaps. Needs to be enhanced for balmo swaps.
#' @param feed Morningstar Feed Table
#' @param contract Tick.prefix in expiry_table e.g. CL for Brent, LCO for Brent
#' @param from Curve date as character string
#' @param iuser Morningstar user name
#' @param ipassword Morningstar user password
#' @return wide data
#' @export getCurve
#' @author Philippe Cote
#' @examples
#' source("~/keys.R")
#' getCurve(feed="CME_NymexFutures_EOD",contract="CL",
#' from="2019-08-27",iuser = mstar[[1]], ipassword = mstar[[2]])

getCurve <- function(feed="CME_NymexFutures_EOD",contract="CL",from="2019-08-27",iuser = "x@xyz.com", ipassword = "pass") {
  # library(RCurl)
  # mpurl <-"https://mp.morningstarcommodity.com/lds/feeds/CME_NymexFutures_EOD/curve?root=CL&cols=Settlement_Price&date=2019-08-27"
  # #mpurl <- "https://mp.morningstarcommodity.com/lds/feeds/CME_NymexFutures_EOD/ts?Symbol=CLZ9"
  # userpw <- paste0(iuser,":",ipassword)
  # URL = mpurl
  # payload = getURL( url =URL, userpw= userpw, ssl.verifypeer = FALSE)
  # out = read.csv(textConnection(payload), header = TRUE, stringsAsFactors = FALSE)
  # return(out)
}

