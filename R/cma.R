#' \code{cma}
#' @description Computes Calendar Month Averages for WTI and Brent and IV for forward starting swaps. Needs to be enhanced for balmo swaps.
#' @param df Wide data frame of date, first contract and second contract
#' @param contract Tick.prefix in expiry_table e.g. CL for Brent, LCO for Brent
#' @param yr Numeric year
#' @param mo Number month
#' @return Wide data frame of date and swap prices.
#' @export cma
#' @author Philippe Cote
#' @examples
#' df <- df_fut %>% dplyr::filter(series %in% c("CL0219","CL0319"))
#' df <- df %>% tidyr::spread(series,value)
#' cma(df=df,contract="CL",yr=2019,mo=01)

cma <- function(df=df_fut,contract="CL",yr=2019,mo=01) {

  if(contract=="CL") {h <- as.Date(dplyr::filter(holidaysOil,key == "nymex")$value)}
  if(contract=="LCO") {h <- as.Date(dplyr::filter(holidaysOil,key == "ice")$value)}

  tmp <-dplyr::filter(expiry_table,tick.prefix==contract,Last.Trade>=as.Date(paste(yr,mo,"01",sep="-")))[1:2,] %>%
      dplyr::mutate(tick=paste(tick.prefix,"_",Year,Month.Letter,sep=""))

  firstday = as.Date(paste(yr,mo,01,sep="-"))
  lastday = lubridate::rollback(firstday + base::months(1))
  expiry = tmp$Last.Trade[1]

  bizdays <- tibble::tibble(date=seq(firstday,lastday,by="day"),DoW=weekdays(date)) %>%
    dplyr::filter(!DoW %in% c("Saturday","Sunday")) %>%
    dplyr::mutate(Up2expiry=ifelse(date <= expiry,1,0)) %>% dplyr::filter(!date %in% h)

  w.c1 = sum(bizdays$Up2expiry) / nrow(bizdays)
  w.c2 = 1-w.c1

  df <- df %>% dplyr::mutate(swap=w.c1*df[,2]+w.c2*df[,3]) %>% dplyr::select(date,swap)

  return(df)
}

