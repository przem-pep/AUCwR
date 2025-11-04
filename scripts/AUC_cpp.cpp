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


// WORK IN PROGRESS
// [[Rcpp::export]]
double auc_cpp(NumericVector pred0, NumericVector pred1) {
  double p0 = pred0.size();
  double p1 = pred1.size();
  double total = 0.0;
  double sum = 0;
  int i = 0;
  int j = 0;
  while(i < p0 && j < p1) {
    if(pred0[i] < pred1[j]) {
      sum = p1 - j;
      total = total + sum;
      i++;
    } else if(pred0[i] == pred1[j]) {
      // WORK IN PROGRESS
      j++;
    } else {
      j++;
    }
  }
  double auc = total / (p0 * p1);
  return auc;
}

// [[Rcpp::export]]
double auc_cpp_trapezoid(NumericVector pred, NumericVector target, double p, int num_uniq) {
  double x = pred.size();
  double n = x - p;
  double tp = 0;
  double fp = 0;
  NumericVector tpr(num_uniq + 1);
  NumericVector fpr(num_uniq + 1);
  double last_pred = std::numeric_limits<double>::infinity();
  double last_tp = 0;
  double last_fp = 0;
  NumericVector area(num_uniq + 1);
  int j = 1;
  for(int i = 0; i < x; i++) {
    last_tp = tp;
    last_fp = fp;
    if(target[i] == 1) {
      tp += 1;
    } else {
      fp += 1;
    }
    if((i > 0 && last_pred != pred[i]) || i == (x - 1)) {
      if(i == (x - 1)) {
        tpr[j] = tp / p;
        fpr[j] = fp / n;
      } else {
        tpr[j] = last_tp / p;
        fpr[j] = last_fp / n;
      }
      area[j] = (fpr[j] - fpr[j - 1]) * (tpr[j] + tpr[j - 1]) * 0.5;
      j += 1;
    }
    last_pred = pred[i];
  }
  double auc = Rcpp::sum(area);
  return auc;
}

