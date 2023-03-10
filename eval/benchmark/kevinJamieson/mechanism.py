from sklearn.linear_model import LogisticRegression
from sklearn.tree import DecisionTreeClassifier
from sklearn.naive_bayes import GaussianNB
from sklearn.multiclass import OneVsRestClassifier
from sklearn.cluster import KMeans
from sklearn.model_selection import GridSearchCV
from sklearn import preprocessing

import math
import numpy as np

from enum import Enum

class Mechanism(Enum):
   GAUSSIAN = 1
   DATASPLIT = 2
   THRESHOLD = 3

class MechanizedLogisticRegression(LogisticRegression):

    def __init__(self, penalty="l2", *, dual=False, tol=0.0001, C=1, 
                 fit_intercept=True, intercept_scaling=1, class_weight=None,
                 random_state=None, solver="lbfgs", max_iter=100, multi_class="auto", 
                 verbose=0, warm_start=False, n_jobs=None, 
                 l1_ratio=None):
        super(MechanizedLogisticRegression, self).__init__(penalty, dual=dual, 
                                                           tol = tol, C = C, fit_intercept = fit_intercept, 
                                                           intercept_scaling = intercept_scaling,
                                                           class_weight = class_weight, 
                                                           random_state = random_state, 
                                                           solver = solver, 
                                                           max_iter = max_iter, 
                                                           multi_class = multi_class, 
                                                           verbose = verbose, 
                                                           warm_start = warm_start, 
                                                           n_jobs = n_jobs, 
                                                           l1_ratio = l1_ratio)
        self.mechanism = None

    def fit(self, x_train, y_train):
        if self.mechanism == None:
            result = super(MechanizedLogisticRegression, self).fit(x_train, y_train)
            if isinstance(result, LogisticRegression):
                return self
            else:
                return result
        elif self.mechanism == Mechanism.GAUSSIAN:
            print("in gaussian mechanism MechanizedLogisticRegression")
            x_noise = np.random.normal(0, 0.1, x_train.shape) 
            noised_x = x_train + x_noise
            
            ################ Gaussian Noise Added to Labels ################
            # y_noise = np.random.normal(0, 0.1, y_train.shape) 
            # noised_y = y_train + y_noise
            # lab = preprocessing.LabelEncoder()
            # y_transformed = pd.Series(
            # lab.fit_transform(noised_y), 
            # y_train.index,
            # y_train.dtype,
            # y_train.name,
            # y_train.copy
            # )
            # y_transformed = lab.fit_transform(noised_y)

            result = super(MechanizedLogisticRegression, self).fit(noised_x, y_train)
            if isinstance(result, GridSearchCV):
                return self
            else:
                return result
            
        else:
            result = super(MechanizedLogisticRegression, self).fit(x_train, y_train)
            if isinstance(result, LogisticRegression):
                return self
            else:
                return result
           
    def choose_mechanism(self, mech):
        self.mechanism = mech




class MechanizedGridSearchCV(GridSearchCV):    
    def __init__(self, estimator, param_grid, *, scoring=None, n_jobs=None, refit=True, cv=None, verbose=0, pre_dispatch="2*n_jobs", error_score=np.nan, return_train_score=False):
        super().__init__(estimator = estimator, 
                         param_grid = param_grid, 
                         scoring=scoring, 
                         n_jobs = n_jobs,
                         refit = refit, 
                         cv = cv, 
                         verbose = verbose, 
                         pre_dispatch = pre_dispatch, 
                         error_score = error_score, 
                         return_train_score = return_train_score)
        self.mechanism = None

    def fit(self, x_train, y_train):
        if self.mechanism == None:
            result = super(MechanizedGridSearchCV, self).fit(x_train, y_train)
            if isinstance(result, GridSearchCV):
                return self
            else:
                return result
        elif self.mechanism == Mechanism.GAUSSIAN:
            print("in gaussian mechanism GridSearchCV")
            x_noise = np.random.normal(0, 0.1, x_train.shape) 
            noised_x = x_train + x_noise
            
            ################ Gaussian Noise Added to Labels ################
            # y_noise = np.random.normal(0, 0.1, y_train.shape) 
            # noised_y = y_train + y_noise
            # lab = preprocessing.LabelEncoder()
            # y_transformed = pd.Series(
            # lab.fit_transform(noised_y), 
            # y_train.index,
            # y_train.dtype,
            # y_train.name,
            # y_train.copy
            # )
            # y_transformed = lab.fit_transform(noised_y)

            result = super(MechanizedGridSearchCV, self).fit(noised_x, y_train)
            if isinstance(result, GridSearchCV):
                return self
            else:
                return result
        else:
            result = super(MechanizedGridSearchCV, self).fit(x_train, y_train)
            if isinstance(result, GridSearchCV):
                return self
            else:
                return result
    
    def choose_mechanism(self, mech):
        self.mechanism = mech


class MechanizedGaussianNB(GaussianNB):
    def __init__(self, *, priors=None, var_smoothing=1e-9):
        super(MechanizedGaussianNB, self).__init__(priors=priors, var_smoothing = var_smoothing) 
        self.mechanism = None


    def fit(self, x_train, y_train):
        if self.mechanism == None:
            result = super(MechanizedGaussianNB, self).fit(x_train, y_train)
            if isinstance(result, GaussianNB):
                return self
            else:
                return result
        elif self.mechanism == Mechanism.GAUSSIAN:
            print("in gaussian mechanism MechanizedGaussianNB")
            x_noise = np.random.normal(0, 0.1, x_train.shape) 
            noised_x = x_train + x_noise
            result = super(MechanizedGaussianNB, self).fit(noised_x, y_train)
            if isinstance(result, GaussianNB):
                return self
            else:
                return result
        else:
            result = super(MechanizedGaussianNB, self).fit(x_train, y_train)
            if isinstance(result, GaussianNB):
                return self
            else:
                return result

    def choose_mechanism(self, mech):
        self.mechanism = mech


class MechanizedKMeans(KMeans):
    def __init__(self, n_clusters=8, *, init="k-means++", n_init="warn", max_iter=300, tol=0.0001, verbose=0, random_state=None, copy_x=True, algorithm="lloyd"):
        super(MechanizedKMeans, self).__init__(
            n_clusters = n_clusters, 
            init = init, 
            n_init = n_init, 
            max_iter = max_iter, 
            tol = tol, 
            verbose = verbose, 
            random_state = random_state, 
            copy_x = copy_x, 
            algorithm = algorithm)
        
        self.mechanism = None


    def fit(self, x_train):
        if self.mechanism == None:
            result = super(MechanizedKMeans, self).fit(x_train)
            if isinstance(result, KMeans):
                return self
            else:
                return result
        elif self.mechanism == Mechanism.GAUSSIAN:
            print("in gaussian mechanism MechanizedGaussianNB")
            x_noise = np.random.normal(0, 0.1, x_train.shape) 
            noised_x = x_train + x_noise
            result = super(MechanizedKMeans, self).fit(noised_x)
            if isinstance(result, KMeans):
                return self
            else:
                return result
        else:
            result = super(MechanizedKMeans, self).fit(x_train)
            if isinstance(result, KMeans):
                return self
            else:
                return result

    def choose_mechanism(self, mech):
        self.mechanism = mech



class MechanizedDecisionTree(DecisionTreeClassifier):
    def __init__(self, *, criterion="gini", splitter="best", max_depth=None, min_samples_split=2, min_samples_leaf=1, min_weight_fraction_leaf=0, max_features=None, random_state=None, max_leaf_nodes=None, min_impurity_decrease=0, class_weight=None, ccp_alpha=0):
        super(MechanizedDecisionTree, self).__init__(
            criterion = criterion, 
            splitter = splitter, max_depth = max_depth, 
            min_samples_split = min_samples_split, 
            min_samples_leaf = min_samples_leaf, min_weight_fraction_leaf = min_weight_fraction_leaf, 
            max_features = max_features, random_state = random_state, max_leaf_nodes = max_leaf_nodes, 
            min_impurity_decrease = min_impurity_decrease, 
            class_weight = class_weight, 
            ccp_alpha = ccp_alpha)
        self.mechanism = None
        

    def fit(self, x_train, y_train):
        if self.mechanism == None:
            result = super(MechanizedDecisionTree, self).fit(x_train, y_train)
            if isinstance(result, DecisionTreeClassifier):
                return self
            else:
                return result
        elif self.mechanism == Mechanism.GAUSSIAN:
            print("in gaussian mechanism MechanizedDecisionTree")
            x_noise = np.random.normal(0, 0.1, x_train.shape) 
            noised_x = x_train + x_noise
            result = super(MechanizedDecisionTree, self).fit(noised_x, y_train)
            if isinstance(result, DecisionTreeClassifier):
                return self
            else:
                return result
        else:
            result = super(MechanizedDecisionTree, self).fit(x_train, y_train)
            if isinstance(result, DecisionTreeClassifier):
                return self
            else:
                return result

    def choose_mechanism(self, mech):
        self.mechanism = mech

class MechanizedOneVSRest(OneVsRestClassifier):
    
    def __init__(self, estimator, *, n_jobs=None, verbose=0):
        super(MechanizedOneVSRest, self).__init__(estimator = estimator, n_jobs = n_jobs, verbose = verbose)
        self.mechanism = None
        

    def fit(self, x_train, y_train):
        if self.mechanism == None:
            result = super(MechanizedOneVSRest, self).fit(x_train, y_train)
            if isinstance(result, OneVsRestClassifier):
                return self
            else:
                return result
        elif self.mechanism == Mechanism.GAUSSIAN:
            print("in gaussian mechanism MechanizedDecisionTree")
            x_noise = np.random.normal(0, 0.1, x_train.shape) 
            noised_x = x_train + x_noise
            result = super(MechanizedOneVSRest, self).fit(noised_x, y_train)
            if isinstance(result, OneVsRestClassifier):
                return self
            else:
                return result
        else:
            result = super(MechanizedOneVSRest, self).fit(x_train, y_train)
            if isinstance(result, OneVsRestClassifier):
                return self
            else:
                return result

    def choose_mechanism(self, mech):
        self.mechanism = mech
