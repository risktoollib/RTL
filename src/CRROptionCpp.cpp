#include <Rcpp.h>
using namespace Rcpp;

// [[Rcpp::export]]
List CRROptionCpp(double S, double X, double sigma, double r, double b, double T2M, int N, std::string type, std::string optionStyle) {
  double dt = T2M / N;
  double u = exp(sigma * sqrt(dt));      // Up factor
  double d = 1/u;                        // Down factor
  double q = (exp(b * dt) - d) / (u - d);
  double discount = exp(-r * dt);

  NumericVector asset_prices(N + 1);
  NumericVector option_values(N + 1);

  for (int i = 0; i <= N; i++) {
    asset_prices[i] = S * pow(u, i) * pow(d, N - i);
    if (type == "call") {
      option_values[i] = std::max(asset_prices[i] - X, 0.0);
    } else if (type == "put") {
      option_values[i] = std::max(X - asset_prices[i], 0.0);
    }
  }

  if (optionStyle != "european" && optionStyle != "american") {
    stop("Invalid option style. Please choose 'european' or 'american'.");
  }

  for (int time_step = N - 1; time_step >= 0; time_step--) {
    for (int j = 0; j <= time_step; j++) {
      double hold_value = (q * option_values[j + 1] + (1 - q) * option_values[j]) * discount;
      if (optionStyle == "american") {
        double exercise_value = (type == "call") ? std::max(S * pow(u, j) * pow(d, time_step - j) - X, 0.0) : std::max(X - S * pow(u, j) * pow(d, time_step - j), 0.0);
        option_values[j] = std::max(hold_value, exercise_value);
      } else {
        option_values[j] = hold_value;
      }
    }
    option_values = option_values[Range(0, time_step)];
  }

  std::string note = (sigma > r * sqrt(dt)) ? "Model parameters are acceptable." : "Consider adjusting your model parameters or using a different model for more accurate results.";

  return List::create(Named("price") = option_values[0], Named("note") = note);
  }
