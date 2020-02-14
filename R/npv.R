#' \code{npv}
#' @description Compute NPV
#' @param init.cost Initial investment cost
#' @param C Periodic cash flow
#' @param cf.freq Cash flow frequency in year fraction e.g. quarterly = 0.25
#' @param TV Terminal Value
#' @param T2M Time to Maturity in years
#' @param disc.factors Data frame of discount factors using ir.df.us() function.
#' @param BreakEven TRUE when using a flat discount rate assumption.
#' @param BE.yield Set the flat IR rate when BeakEven = TRUE.
#' @return List of NPV and NPV Data frame
#' @export npv
#' @author Philippe Cote
#' @examples
#' \dontrun{
#' us.df <- ir_df_us(quandlkey = quandlkey,ir.sens=0.01)
#' npv(init.cost=-375,C=50,cf.freq=.5,TV=250,T2M=2,
#' disc.factors=us.df,BreakEven=TRUE,BE.yield=.0399)$npv
#' npv(init.cost=-375,C=50,cf.freq=.5,TV=250,T2M=2,
#' disc.factors=us.df,BreakEven=TRUE,BE.yield=.0399)$df
#' }
npv <- function(init.cost=-375,C=50,cf.freq=.25,TV=250,T2M=2,disc.factors=us.df,BreakEven=FALSE,BE.yield=.01) {
  if (BreakEven==TRUE) {
    disc.factors$yield <- BE.yield
    disc.factors <- disc.factors %>% dplyr::mutate(discountfactor=exp(-yield*maturity))
  }
  df <- dplyr::tibble(t=seq(from=0,to=T2M,by=cf.freq),cf=C) %>%
    dplyr::mutate(cf=replace(cf,t==0,init.cost),
                  cf=replace(cf,t==T2M,TV),
                  df=stats::spline(x=disc.factors$maturity,y=disc.factors$discountfactor,xout=t)$y,
                  pv=cf*df)
  x=list(df=df,npv=sum(df$pv))
  return(x)
}
