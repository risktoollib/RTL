#include <Rcpp.h>
using namespace Rcpp;

// [[Rcpp::export]]
RObject rcppQT(NumericMatrix out, NumericMatrix prices ,Function tradeStrategy, Function tradeStats) {
  return(tradeStrategy(_["data"] = prices, out[1, 0], out[1, 1])));
}





