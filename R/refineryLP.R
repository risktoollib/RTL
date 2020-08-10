#' \code{refineryLP}
#' @description Plain vanilla refinery optmization LP model.
#' @param crudes Data frame of crude inputs
#' @param products Data frame of product outputs and max outputs.
#' @return Optimal crude slate and profits
#' @export refineryLP
#' @author Philippe Cote
#' @examples
#' refineryLP(crudes = ref.opt.inputs, products = ref.opt.outputs)
refineryLP <- function(crudes = ref.opt.inputs, products = ref.opt.outputs) {
  requireNamespace("lpSolve", quietly=TRUE)
    GPW <- tibble::tibble(Element=c("Gross.Product.Worth","Crude.Cost","Processing"),
                                LightSweet=c(products %>% dplyr::transmute(LightSweet = prices*LightSweet.yield) %>% stats::na.omit() %>% sum(),
                                             crudes %>% dplyr::filter(info == "price") %>% dplyr::select(LightSweet) %>% as.numeric(),
                                             crudes %>% dplyr::filter(info == "processing.fee") %>% dplyr::select(LightSweet) %>% as.numeric()),
                                HeavySour=c(products %>% dplyr::transmute(HeavySour=prices*HeavySour.yield) %>% stats::na.omit() %>% sum(),
                                            crudes %>% dplyr::filter(info == "price") %>% dplyr::select(HeavySour) %>% as.numeric(),
                                            crudes %>% dplyr::filter(info == "processing.fee") %>% dplyr::select(HeavySour) %>% as.numeric()))
  GPW <- GPW %>% dplyr::select(-Element) %>% colSums()
  constraints <- products %>% dplyr::select(product,LightSweet.yield,HeavySour.yield,max.prod)
  out <- lpSolve::lp(direction = "max",
              objective.in = as.numeric(GPW),
              const.mat = as.matrix(constraints[,2:3]),
              const.dir = c(rep("<=",4)),
              const.rhs = as.matrix(constraints[,4]),
              all.int = FALSE,compute.sens = 1)
  out <-  list(profit = out$objval, slate = out$solution)
  return(out)
}
