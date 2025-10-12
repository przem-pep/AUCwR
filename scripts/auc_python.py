from sklearn.metrics import roc_auc_score

def scikitlearn_auc(pred, target):
    return roc_auc_score(target, pred)
