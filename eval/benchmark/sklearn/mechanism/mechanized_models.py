from sklearn.linear_model import LogisticRegression
from sklearn.tree import DecisionTreeClassifier
from sklearn.naive_bayes import GaussianNB
from sklearn.multiclass import OneVsRestClassifier
from sklearn.cluster import KMeans
import math


class MechanizedLogisticRegression(LogisticRegression):
    def __init__(self, *args, **kwargs):
        super(MechanizedLogisticRegression, self).__init__(*args, **kwargs)

    def fit(self, x_train, y_train):
        super().fit(x_train + math.normal(0, 0.1), y_train)
    

class MechanizedGaussianNB(GaussianNB):
    def __init__(self, *args, **kwargs):
        super(MechanizedGaussianNB, self).__init__(*args, **kwargs)

    def fit(self, x_train, y_train):
        super().fit(x_train + math.normal(0, 0.1), y_train)

class MechanizedKMeans(KMeans):
    def __init__(self, *args, **kwargs):
        super(MechanizedKMeans, self).__init__(*args, **kwargs)

    def fit(self, x_train, y_train):
        super().fit(x_train + math.normal(0, 0.1), y_train)


class MechanizedDecisionTree(DecisionTreeClassifier):
    def __init__(self, *args, **kwargs):
        super(MechanizedDecisionTree, self).__init__(*args, **kwargs)

    def fit(self, x_train, y_train):
        super().fit(x_train + math.normal(0, 0.1), y_train)

class MechanizedOneVSRest(OneVsRestClassifier):
    def __init__(self, *args, **kwargs):
        super(MechanizedOneVSRest, self).__init__(*args, **kwargs)

    def fit(self, x_train, y_train):
        super().fit(x_train + math.normal(0, 0.1), y_train)

