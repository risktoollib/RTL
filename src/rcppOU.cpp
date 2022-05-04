#include <Rcpp.h>
using namespace Rcpp;

// [[Rcpp::export]]
NumericMatrix rcppOU(NumericMatrix x, double theta, double mu, double dt, double sigma) {
  for (int i = 1; i < x.nrow(); i++) {
    for (int j = 0; j < x.ncol(); j++) {
      x(i,j) =  x(i-1,j) + theta * (mu - x(i-1,j)) * dt + sigma * x(i,j) ;
    }
  }
  return x;
}
