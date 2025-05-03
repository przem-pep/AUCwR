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