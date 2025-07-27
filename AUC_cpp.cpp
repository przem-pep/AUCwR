#include <Rcpp.h>

using namespace Rcpp;

// [[Rcpp::export]]
double AUC_calc(NumericVector pred, NumericVector target, NumericVector n, NumericVector negatives) {
    int num = pred.size();
    double P = sum(target);
    double N = sum(negatives);
    double TP = 0.0;
    double FP = 0.0;
    NumericVector TP_vec(num);
    NumericVector FP_vec(num);
    NumericVector TPR(num);
    NumericVector FPR(num);
    for(int i = 0; i < num; i++) {
        TP = TP + target[i];
        FP = FP + negatives[i];
        TP_vec[i] = TP;
        FP_vec[i] = FP;
        TPR[i] = TP / P;
        FPR[i] = FP / N;
    }
    NumericVector h(num - 1);
    NumericVector area(num - 1);
    for(int j = 0; j < num - 1; j++) {
        h[j] = FPR[j + 1] - FPR[j];
        area[j] = (TPR[j] + TPR[j + 1]) * h[j] * 0.5;
    }
    return sum(area);
}

// [[Rcpp::export]]
double own_AUC_cpp(NumericVector pred, IntegerVector label) {
  int n = pred.size();
  std::vector<std::pair<double, int>> data(n);
  
  // Combine pred and label into one sortable structure
  for (int i = 0; i < n; ++i) {
    data[i] = std::make_pair(pred[i], label[i]);
  }
  
  // Sort descending by predicted probability
  std::sort(data.begin(), data.end(), [](const std::pair<double, int>& a, const std::pair<double, int>& b) {
    return a.first > b.first;
  });
  
  // Rank-based approach for AUC
  double cum_pos = 0.0;
  double cum_neg = 0.0;
  double auc = 0.0;
  
  for (int i = 0; i < n; ++i) {
    if (data[i].second == 1) {
      cum_pos += 1;
    } else {
      auc += cum_pos;
      cum_neg += 1;
    }
  }
  
  if (cum_pos == 0 || cum_neg == 0) return NA_REAL;
  
  return auc / (cum_pos * cum_neg);
}

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

