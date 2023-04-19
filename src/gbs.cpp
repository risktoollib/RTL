#include <Rcpp.h>
#ifndef Pi
#define Pi 3.141592653589793238462643
#endif
using namespace Rcpp;

// [[Rcpp::export]]
double CND(double x) {
  double L, K, w ;

  double const a1 = 0.31938153, a2 = -0.356563782, a3 = 1.781477937;
  double const a4 = -1.821255978, a5 = 1.330274429;

  L = fabs(x);
  K = 1.0 / (1.0 + 0.2316419 * L);
  w = 1.0 - 1.0 / sqrt(2 * Pi) * exp(-L *L / 2) * (a1 * K + a2 * K *K + a3 * pow(K,3) + a4 * pow(K,4) + a5 * pow(K,5));

  if (x < 0 ){
    w= 1.0 - w;
  }
  return w;
  return x * 2;
}

// [[Rcpp::export]]
double gbs(char CallPutFlag, double S, double X, double T, double r, double v) {
  double d1, d2;

  d1 = (log(S/X) + (r + v * v/2) * T) / (v * sqrt(T));
  d2 = d1 - v * sqrt(T);

  if (CallPutFlag == 'c')
    return S *CND(d1)-X * exp(-r*T)*CND(d2);
  else if (CallPutFlag == 'p')
    return X * exp(-r * T) * CND(-d2) - S * CND(-d1);
  else
    return 0;
}



