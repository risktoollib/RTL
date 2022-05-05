#include <Rcpp.h>
using namespace Rcpp;

// [[Rcpp::export]]
NumericMatrix rcppOUt(NumericMatrix x, double theta, double dt, double sigma) {
  for (int i = 1; i < x.nrow(); i++) {
    for (int j = 1; j < x.ncol(); j++) {
      x(i,j) =  x(i-1,j) + theta * (x(i,0) - x(i-1,j)) * dt + sigma * x(i,j) ;
    }
  }
  return x;
}
