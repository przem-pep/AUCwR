#include <Rcpp.h>

using namespace Rcpp;

// [[Rcpp::export]]
double auc_pairwise_cpp(NumericVector pred0, NumericVector pred1) {
  double p0 = pred0.size();
  double p1 = pred1.size();
  double total = 0.0;
  int i = 0;
  int j = 0;
  while(i < p0 && j < p1) {
    if(pred0[i] < pred1[j]) {
      total = total + p1 - j;
      i++;
    } else if(pred0[i] == pred1[j]) {
      int i_start = i;
      int j_start = j;
      while(i < p0 && pred0[i] == pred0[i_start]) {
        i++;
      }
      while(j < p1 && pred1[j] == pred1[j_start]) {
        j++;
      }
      total = total + 0.5 * (i - i_start) * (j - j_start) + (i - i_start) * (p1 - j);
    } else {
      j++;
    }
  }
  double auc = total / (p0 * p1);
  return auc;
}

// [[Rcpp::export]]
double auc_trapezoid_cpp(NumericVector pred, NumericVector target, double p, int num_uniq) {
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
