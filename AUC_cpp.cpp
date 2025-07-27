#include <Rcpp.h>

using namespace Rcpp;

// [[Rcpp::export]]
double own_AUC_U_cpp(NumericVector pred, NumericVector target) {
  int n = pred.size();
  std::vector<std::pair<double, int>> data(n);
  for(int i = 0; i < n; i++) {
    data[i] = std::make_pair(pred[i], target[i]);
  }
  std::sort(data.begin(), data.end());
  NumericVector ranks(n);
  double i = 0;
  double j;
  while(i < n) {
    if(i + 1 < n && data[i].first == data[i + 1].first) {
      j = i;
      while(j + 1 < n && data[j].first == data[j + 1].first) {
        j++;
      }
      for(int k = i; k <= j; k++) {
        ranks[k] = ((i + 1) + (j + 1)) / 2;
      }
      i = j + 1;
    }
    else {
      ranks[i] = i + 1;
      i++;
    }
  }
  int n1 = 0;
  int n2 = 0;
  double R1 = 0.0;
  for(int i = 0; i < n; i++) {
    if(data[i].second == 0) {
      n1 = n1 + 1;
      R1 = R1 + ranks[i];
    }
    else {
      n2 = n2 + 1;
    }
  }
  double U1 = n1 * n2 + ((n1 * (n1 + 1)) / 2) - R1;
  double AUC = U1 / (n1 * n2);
  return AUC;
}

