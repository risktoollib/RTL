#include <Rcpp.h>
using namespace Rcpp;

// Mean-reverting jump-diffusion of Clewlow & Strickland (2000), eq. 2.17, stepped in x = ln S (eq. 2.3):
//   x(i) = x(i-1) + [theta (mu - x(i-1)) - sigma^2 / 2] dt + sigma dz(i) + djump(i)
// x: row 0 holds ln S0, rows 1.. hold the Brownian increments dz; djump: the sum of ln(1 + kappa) over the jumps
// in each step; mu: ln of the long-term level. Returns ln S.
// [[Rcpp::export]]
NumericMatrix rcppOUJ(NumericMatrix x, NumericMatrix djump, double theta, double mu, double dt, double sigma) {
  for (int i = 1; i < x.nrow(); i++) {
    for (int j = 0; j < x.ncol(); j++) {
      x(i,j) = x(i-1,j) + (theta * (mu - x(i-1,j)) - 0.5 * sigma * sigma) * dt + sigma * x(i,j) + djump(i,j);
    }
  }
  return x;
}
