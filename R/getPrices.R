#' \code{getPrices}
#' @description Returns data from Morningstat API
#' @param feed Morningstar Feed Table
#' @param contracts Symbols vector
#' @param from From date as character string
#' @param iuser Morningstar user name
#' @param ipassword Morningstar user password
#' @return wide data frame
#' @export getPrices
#' @author Philippe Cote
#' @examples
#' source("~/keys.R")
#' getPrices(feed="CME_NymexFutures_EOD",contracts=c("CL9Z","CL0F","CL0M"),
#' from="2019-08-26",iuser = mstar[[1]], ipassword = mstar[[2]])

getPrices <- function(feed="CME_NymexFutures_EOD",contracts=c("CL9Z","CL0F","CL0M"),from="2019-01-01",iuser = "x@xyz.com", ipassword = "pass") {

  x <- getPrice(feed=feed,contract=contracts[1],from=from,iuser = iuser, ipassword = ipassword)
  for (c in contracts[-1]) {
    x <- merge(x,getPrice(feed=feed,contract=c,from=from,iuser = iuser, ipassword = ipassword))
  }
  return(x)
}
