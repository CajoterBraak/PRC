#include <Rcpp.h>
using namespace Rcpp;

// This is a simple example of exporting a C++ function to R. You can
// source this function into an R session using the Rcpp::sourceCpp
// function (or via the Source button on the editor toolbar). Learn
// more about Rcpp at:
//
//   http://www.rcpp.org/
//   http://adv-r.had.co.nz/Rcpp.html
//   http://gallery.rcpp.org/
//

// [[Rcpp::export]]
NumericVector A_mult_dist(NumericVector D,
                          NumericVector x)
{
  // const int n =    (1 + std::sqrt(1 + 8 * D.size())) / 2;
  const int n = x.size();

  NumericVector y(n);

  int k = 0;

  for (int j = 0; j < n - 1; ++j) {
    for (int i = j + 1; i < n; ++i, ++k) {

      const double v = D[k];

      y[i] += v * x[j];
      y[j] += v * x[i];
    }
  }

  return y;
}


