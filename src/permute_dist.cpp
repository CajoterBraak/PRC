#include <Rcpp.h>
using namespace Rcpp;

// [[Rcpp::export]]
NumericVector permute_dist_cpp(NumericVector D,
                               IntegerVector perm,
                               int n)
{
  int m = D.size();
  NumericVector out(m);

  int k = 0;

  for (int i = 0; i < n - 1; ++i) {
    for (int j = i + 1; j < n; ++j) {

      int a = perm[i];
      int b = perm[j];

      if (a > b)
        std::swap(a, b);

      // dist index (1-based formula converted to
      // 0-based C++ vector indexing)
      int idx =
        n * (a - 1)
        - (a * (a - 1)) / 2
      + (b - a)
      - 1;

      out[k++] = D[idx];
    }
  }

  return out;
}
