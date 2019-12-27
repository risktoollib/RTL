#' \code{getPrice}
#' @description Returns data from Morningstat API
#' @param feed Morningstar Feed Table
#' @param contract Morningstar key. When multiples keys are required under a data feed, separate them by a space. For exaample the CFTC_CommitmentsOfTradersCombined feed requires four keys and would written as "N10 06765A NYME 01".
#' @param from From date as character string
#' @param iuser Morningstar user name
#' @param ipassword Morningstar user password
#' @return wide data frame
#' @export getPrice
#' @author Philippe Cote
#' @examples
#' source("~/keys.R")
#' getPrice(feed="CME_NymexFutures_EOD",contract="CL9Z",
#' from="2019-08-26",iuser = mstar[[1]], ipassword = mstar[[2]])
#' getPrice(feed="CME_NymexFutures_EOD_continuous",contract="CL_006_Month",
#' from="2019-08-26",iuser = mstar[[1]], ipassword = mstar[[2]])
#' getPrice(feed="CME_CbotFuturesEOD",contract="C9Z",
#' from="2019-08-26",iuser = mstar[[1]], ipassword = mstar[[2]])
#' getPrice(feed="CME_CbotFuturesEOD_continuous",contract="ZB_001_Month",
#' from="2019-08-26",iuser = mstar[[1]], ipassword = mstar[[2]])
#' getPrice(feed="CME_CmeFutures_EOD_continuous",contract="HE_006_Month",
#' from="2019-08-26",iuser = mstar[[1]], ipassword = mstar[[2]])
#' getPrice(feed="Morningstar_FX_Forwards",contract="USDCAD, 2M",
#' from="2019-08-26",iuser = mstar[[1]], ipassword = mstar[[2]])
#' getPrice(feed="CME_CmeFutures_EOD",contract="LH0N",
#' from="2019-08-26",iuser = mstar[[1]], ipassword = mstar[[2]])
#' getPrice(feed="CME_CmeFutures_EOD_continuous",contract="HE_006_Month",
#' from="2019-08-26",iuser = mstar[[1]], ipassword = mstar[[2]])
#' getPrice(feed="ICE_EuroFutures",contract="BRN0Z",
#' from="2019-08-26",iuser = mstar[[1]], ipassword = mstar[[2]])
#' getPrice(feed="ICE_EuroFutures_continuous",contract="BRN_001_Month",
#' from="2019-08-26",iuser = mstar[[1]], ipassword = mstar[[2]])
#' getPrice(feed="ICE_NybotCoffeeSugarCocoaFutures",contract="SB0H",
#' from="2019-08-26",iuser = mstar[[1]], ipassword = mstar[[2]])
#' getPrice(feed="ICE_NybotCoffeeSugarCocoaFutures_continuous",contract="SF_001_Month",
#' from="2019-08-26",iuser = mstar[[1]], ipassword = mstar[[2]])

getPrice <- function(feed="CME_NymexFutures_EOD",contract="CL9Z",from="2019-01-01",iuser = "x@xyz.com", ipassword = "pass") {
  #mpurl <- "https://mp.morningstarcommodity.com/lds/feeds/CME_NymexFutures_EOD/ts?Symbol=CL9Z"
  userpw <- paste0(iuser,":",ipassword)
  if (feed %in% c("CME_NymexFutures_EOD","CME_CbotFuturesEOD","CME_CmeFutures_EOD","ICE_EuroFutures","ICE_NybotCoffeeSugarCocoaFutures")) {
    URL = httr::modify_url(url = "https://mp.morningstarcommodity.com",path = paste0("/lds/feeds/",feed, "/ts?","Symbol=",contract,"&fromDateTime=",from))}
  if (feed %in% c("CME_NymexFutures_EOD_continuous","CME_CbotFuturesEOD_continuous","CME_CmeFutures_EOD_continuous","ICE_EuroFutures_continuous","ICE_NybotCoffeeSugarCocoaFutures_continuous")) {
    URL = httr::modify_url(url = "https://mp.morningstarcommodity.com",path = paste0("/lds/feeds/",feed, "/ts?","Contract=",contract,"&fromDateTime=",from))}
  if (feed %in% c("CME_STLCPC_Futures")) {URL = httr::modify_url(url = "https://mp.morningstarcommodity.com",path = paste0("/lds/feeds/",feed, "/ts?","product=",contract,"&fromDateTime=",from))}
  if (feed %in% c("CFTC_CommitmentsOfTradersCombined")) {
    URL = httr::modify_url(url = "https://mp.morningstarcommodity.com",
                           path = paste0("/lds/feeds/",feed, "/ts?",
                                         "cftc_subgroup_code=",stringr::word(contract,1,1),
                                         "&cftc_contract_market_code=",stringr::word(contract,2,2),
                                         "&cftc_market_code=",stringr::word(contract,3,3),
                                         "&cftc_region_code=",stringr::word(contract,4,4),
                                         "&cols=",stringr::word(contract,5,5),
                                         "&fromDateTime=",from))}

  if (feed=="Morningstar_FX_Forwards") {
    x = strsplit(gsub(" ","",contract),",")
    x1 = x[[1]][1]
    x2 = x[[1]][2]
    URL = httr::modify_url(url = "https://mp.morningstarcommodity.com",path = paste0("/lds/feeds/",feed, "/ts?","cross_currencies=",x1,"&period=",x2,"&fromDateTime=",from))
    }
  httr::handle_reset(URL)
  es <- httr::GET(url = URL,httr::authenticate(user = iuser,password = ipassword,type = "basic")) #,httr::progress())

  if(length(httr::content(es))>0) {
    if(length(httr::content(es) %>% purrr::flatten() %>% .$series %>% .$values %>% purrr::flatten()) > 0) {
      out<-dplyr::tibble(date=as.character(lubridate::ymd(httr::content(es) %>%
                                                         purrr::flatten() %>% purrr::flatten() %>%
                                                         .$dates)) %>% lubridate::ymd(),
                      value=as.numeric(httr::content(es) %>%
                                         purrr::flatten() %>% purrr::flatten() %>%
                                         .$values %>% .[[1]] %>%
                                         purrr::flatten())) %>%
        dplyr::mutate(value=ifelse(is.nan(value),NA,value)) } else {
          out=dplyr::tibble(date=character(),value=numeric(),fwdmnt=numeric(),fwdyr=numeric())}} else {
            out=dplyr::tibble(date=character(),value=numeric(),fwdmnt=numeric(),fwdyr=numeric())}
  if (length(colnames(out))==2) {colnames(out)[2] <- contract}
  return(out)
}
