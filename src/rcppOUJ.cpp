#include <Rcpp.h>
using namespace Rcpp;

// [[Rcpp::export]]
NumericMatrix rcppOUJ(NumericMatrix x, NumericMatrix djump, double theta, double mu, double dt, double sigma, double jump_prob, double jump_avesize) {
  for (int i = 1; i < x.nrow(); i++) {
    for (int j = 0; j < x.ncol(); j++) {
      x(i,j) =  x(i-1,j) + theta * (mu - (jump_prob * jump_avesize) - x(i-1,j)) * dt + sigma * x(i,j) + djump(i,j);
    }
  }
  return x;
}
