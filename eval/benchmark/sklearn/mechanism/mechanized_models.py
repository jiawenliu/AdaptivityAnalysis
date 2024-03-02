from sklearn.linear_model import LogisticRegression
from sklearn.tree import DecisionTreeClassifier
from sklearn.naive_bayes import GaussianNB
from sklearn.multiclass import OneVsRestClassifier
from sklearn.cluster import KMeans
from sklearn.model_selection import GridSearchCV
from sklearn import preprocessing
from sklearn.metrics import accuracy_score, f1_score, roc_auc_score, confusion_matrix, RocCurveDisplay

import math
import numpy as np

from enum import Enum



class Mechanism():
    class MechanismType(Enum):
        NONE = 0
        GAUSSIAN = 1
        DATASPLIT = 2
        THRESHOLD = 3

    def __init__(self, mechanism_type = MechanismType.NONE, mu = 0.0, sigma = 0.1, hold_frac = 0.5, threshold = 0.5, beta = None, tau = None, check_for_width = None):

        super().__init__()
        self.mechanism_type = mechanism_type
        '''
        Parameters for the Gaussian mechanism 
        '''
        self.mu = mu
        self.sigma = sigma

        '''
        Parameters for the GnC mechanism 
        '''      
        self.beta = beta
        self.tau = tau
        self.check_for_width = check_for_width


        '''
        Parameters for the Naive Data Splitting mechanism 
        '''
        self.split_size = None


        '''
        Parameters for the Thresholdout mechanism 
        '''
        self.hold_size = None
        self.train_size = None 
        assert 0.0 < hold_frac <= 1.0, "hold_frac should take a value in (0, 1]."
        self.hold_frac = hold_frac
        self.threshold = threshold
        self.noisy_thresh = self.threshold + np.random.laplace(0, 2 * self.sigma)

    def initialize_with_str_seed(init_str):
        """
        Initializes random number generator with seed corresponding to given input string init_str.
        :param init_str: Initialization string according to which seed will be computed. Seed is the sum of the ASCII
                        values of each character in init_str.
        """
        rnd_val = 0
        if init_str:
            for c in init_str:
                rnd_val += ord(c)
        np.random.seed(rnd_val)
from sklearn.multiclass import OneVsRestClassifier



class MechanizedDecisionTree(DecisionTreeClassifier):
    def __init__(self, *, criterion="gini", splitter="best", max_depth=None, min_samples_split=2, min_samples_leaf=1, min_weight_fraction_leaf=0, max_features=None, random_state=None, max_leaf_nodes=None, min_impurity_decrease=0, class_weight=None, ccp_alpha=0,
                 mechanism = Mechanism(Mechanism.MechanismType.NONE)):
        super(MechanizedDecisionTree, self).__init__(
            criterion = criterion, 
            splitter = splitter, max_depth = max_depth, 
            min_samples_split = min_samples_split, 
            min_samples_leaf = min_samples_leaf, min_weight_fraction_leaf = min_weight_fraction_leaf, 
            max_features = max_features, random_state = random_state, max_leaf_nodes = max_leaf_nodes, 
            min_impurity_decrease = min_impurity_decrease, 
            class_weight = class_weight, 
            ccp_alpha = ccp_alpha)
        self.mechanism = mechanism

            
    def choose_mech(self, mech = None):
        self.mechanism = mech


    def fit_data_split(self, x_train, y_train):
        size = len(x_train)
        hold_size, train_size = int(size  * (self.mechanism.hold_frac)), int(size  * (1.0 - self.mechanism.hold_frac))
        x_train, y_train, x_hold, y_hold = x_train[hold_size:], y_train[hold_size:], x_train[:hold_size], y_train[:hold_size]
        train_result = super(MechanizedDecisionTree, self).fit(x_train, y_train)
        train_pred = train_result.predict(x_train)
        hold_result = super(MechanizedDecisionTree, self).fit(x_hold, y_hold)
        hold_pred = hold_result.predict(x_hold)
        if abs(accuracy_score(train_pred, y_train) - accuracy_score(hold_pred, y_hold)) >= self.mechanism.noisy_thresh + np.random.laplace(0, 4 * self.mechanism.sigma):
            self.mechanism.noisy_thresh = self.mechanism.threshold + np.random.laplace(0, 2 * self.mechanism.sigma)
            x_noise =  np.random.laplace(0, 2 * self.mechanism.sigma, x_hold.shape)
            return super(MechanizedDecisionTree, self).fit(x_hold + x_noise, y_hold)
        else:
            return train_result

    def fit_threshold(self, x_train, y_train):
        size = len(x_train)
        hold_size, train_size = int(size  * (self.mechanism.hold_frac)), int(size  * (1.0 - self.mechanism.hold_frac))
        x_train, y_train, x_hold, y_hold = x_train[hold_size:], y_train[hold_size:], x_train[:hold_size], y_train[:hold_size]
        train_result = super(MechanizedDecisionTree, self).fit(x_train, y_train)
        train_pred = train_result.predict(x_train)
        hold_result = super(MechanizedDecisionTree, self).fit(x_hold, y_hold)
        hold_pred = hold_result.predict(x_hold)
        if abs(accuracy_score(train_pred, y_train) - accuracy_score(hold_pred, y_hold)) >= self.mechanism.noisy_thresh + np.random.laplace(0, 4 * self.mechanism.sigma):
            self.mechanism.noisy_thresh = self.mechanism.threshold + np.random.laplace(0, 2 * self.mechanism.sigma)
            x_noise =  np.random.laplace(0, 2 * self.mechanism.sigma, x_hold.shape)
            return super(MechanizedDecisionTree, self).fit(x_hold + x_noise, y_hold)
        else:
            return train_result


    def fit_gaussian(self, x_train, y_train):
        x_noise = np.random.normal(0, self.mechanism.sigma, x_train.shape) 
        noised_x = x_train + x_noise        
        
        ################ Gaussian Noise Added to Labels ################
        y_noise = np.random.normal(0, self.mechanism.sigma, y_train.shape) 
        noised_y = y_train


        result = super(MechanizedDecisionTree, self).fit(noised_x, noised_y)
        if isinstance(result, DecisionTreeClassifier):
            return self
        else:
            return result



    def fit(self, x_train, y_train):
        if self.mechanism.mechanism_type ==  Mechanism.MechanismType.NONE:
            print("in Baseline Logistic Regression")
            result = super(MechanizedDecisionTree, self).fit(x_train, y_train)
            if isinstance(result, DecisionTreeClassifier):
                return self
            else:
                return result
        elif self.mechanism.mechanism_type ==  Mechanism.MechanismType.GAUSSIAN:
            print("in Gaussian Mechanized Logistic Regression")
            return self.fit_gaussian(x_train, y_train)
        
        elif self.mechanism.mechanism_type ==  Mechanism.MechanismType.THRESHOLD:
            print("in Threshold Mechanized Logistic Regression")
            return self.fit_threshold(x_train, y_train)
            
        else:
            result = super(MechanizedDecisionTree, self).fit(x_train, y_train)
            if isinstance(result, DecisionTreeClassifier):
                return self
            else:
                return result
           
    def choose_mechanism(self, mech):
        self.mechanism = mech



class MechanizedLogisticRegression(LogisticRegression):

    def __init__(self, penalty="l2", *, dual=False, tol=0.0001, C=1, 
                 fit_intercept=True, intercept_scaling=1, class_weight=None,
                 random_state=None, solver="lbfgs", max_iter=100, multi_class="auto", 
                 verbose=0, warm_start=False, n_jobs=None, 
                 l1_ratio=None,
                 mechanism = Mechanism(Mechanism.MechanismType.NONE)):
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
        self.mechanism = mechanism

    def fit_data_split(self, x_train, y_train):
        size = len(x_train)
        hold_size, train_size = int(size  * (self.mechanism.hold_frac)), int(size  * (1.0 - self.mechanism.hold_frac))
        x_train, y_train, x_hold, y_hold = x_train[hold_size:], y_train[hold_size:], x_train[:hold_size], y_train[:hold_size]
        train_result = super(MechanizedLogisticRegression, self).fit(x_train, y_train)
        train_pred = train_result.predict(x_train)
        hold_result = super(MechanizedLogisticRegression, self).fit(x_hold, y_hold)
        hold_pred = hold_result.predict(x_hold)
        if abs(accuracy_score(train_pred, y_train) - accuracy_score(hold_pred, y_hold)) >= self.mechanism.noisy_thresh + np.random.laplace(0, 4 * self.mechanism.sigma):
            self.mechanism.noisy_thresh = self.mechanism.threshold + np.random.laplace(0, 2 * self.mechanism.sigma)
            x_noise =  np.random.laplace(0, 2 * self.mechanism.sigma, x_hold.shape)
            return super(MechanizedLogisticRegression, self).fit(x_hold + x_noise, y_hold)
        else:
            return train_result

    def fit_threshold(self, x_train, y_train):
        size = len(x_train)
        hold_size, train_size = int(size  * (self.mechanism.hold_frac)), int(size  * (1.0 - self.mechanism.hold_frac))
        x_train, y_train, x_hold, y_hold = x_train[hold_size:], y_train[hold_size:], x_train[:hold_size], y_train[:hold_size]
        train_result = super(MechanizedLogisticRegression, self).fit(x_train, y_train)
        train_pred = train_result.predict(x_train)
        hold_result = super(MechanizedLogisticRegression, self).fit(x_hold, y_hold)
        hold_pred = hold_result.predict(x_hold)
        if abs(accuracy_score(train_pred, y_train) - accuracy_score(hold_pred, y_hold)) >= self.mechanism.noisy_thresh + np.random.laplace(0, 4 * self.mechanism.sigma):
            self.mechanism.noisy_thresh = self.mechanism.threshold + np.random.laplace(0, 2 * self.mechanism.sigma)
            x_noise =  np.random.laplace(0, 2 * self.mechanism.sigma, x_hold.shape)
            return super(MechanizedLogisticRegression, self).fit(x_hold + x_noise, y_hold)
        else:
            return train_result


    def fit_gaussian(self, x_train, y_train):
        x_noise = np.random.normal(0, self.mechanism.sigma, x_train.shape) 
        noised_x = x_train + x_noise        
        
        ################ Gaussian Noise Added to Labels ################
        y_noise = np.random.normal(0, self.mechanism.sigma, y_train.shape) 
        noised_y = y_train  + y_noise
        noised_y = list(map(lambda x : 0 if x < 0 else 1, noised_y))
        # lab_enc = preprocessing.LabelEncoder()
        # noised_y = lab_enc.fit_transform(noised_y) 
        # print(noised_y)


        result = super(MechanizedLogisticRegression, self).fit(noised_x, noised_y)
        if isinstance(result, LogisticRegression):
            # print("yes, it is logistic regression")
            return self
        else:
            # print("no, it is result from Mechanized logistic regression")
            return result



    def fit(self, x_train, y_train):
        if self.mechanism.mechanism_type ==  Mechanism.MechanismType.NONE:
            print("in Baseline Logistic Regression")
            result = super(MechanizedLogisticRegression, self).fit(x_train, y_train)
            if isinstance(result, LogisticRegression):
                return self
            else:
                return result
        elif self.mechanism.mechanism_type ==  Mechanism.MechanismType.GAUSSIAN:
            print("in Gaussian Mechanized Logistic Regression")
            return self.fit_gaussian(x_train, y_train)
        
        elif self.mechanism.mechanism_type ==  Mechanism.MechanismType.THRESHOLD:
            print("in Threshold Mechanized Logistic Regression")
            return self.fit_threshold(x_train, y_train)
        elif self.mechanism.mechanism_type ==  Mechanism.MechanismType.DATASPLIT:
            print("in DataSplit Mechanized Logistic Regression")
            return self.fit_data_split(x_train, y_train)
        else:
            result = super(MechanizedLogisticRegression, self).fit(x_train, y_train)
            if isinstance(result, LogisticRegression):
                return self
            else:
                return result
           
    def choose_mechanism(self, mech):
        self.mechanism = mech


class MechanizedOneVSRest(OneVsRestClassifier):
    
    def __init__(self, estimator, *, n_jobs=None, verbose=0,
                 mechanism = Mechanism(Mechanism.MechanismType.NONE)):
        super(MechanizedOneVSRest, self).__init__(estimator = estimator, n_jobs = n_jobs, verbose = verbose)
        self.mechanism = mechanism
    
    def fit_threshold(self, x_train, y_train):
        size = len(x_train)
        hold_size, train_size = int(size  * (self.mechanism.hold_frac)), int(size  * (1.0 - self.mechanism.hold_frac))
        x_train, y_train, x_hold, y_hold = x_train[hold_size:], y_train[hold_size:], x_train[:hold_size], y_train[:hold_size]
        train_result = super(MechanizedOneVSRest, self).fit(x_train, y_train)
        train_pred = train_result.predict(x_train)
        hold_result = super(MechanizedOneVSRest, self).fit(x_hold, y_hold)
        hold_pred = hold_result.predict(x_hold)
        if abs(accuracy_score(train_pred, y_train) - accuracy_score(hold_pred, y_hold)) >= self.mechanism.noisy_thresh + np.random.laplace(self.mechanism.mu, 4 * self.mechanism.sigma):
            self.mechanism.noisy_thresh = self.mechanism.threshold + np.random.laplace(self.mechanism.mu, 2 * self.mechanism.sigma)
            x_noise =  np.random.laplace(self.mechanism.mu, self.mechanism.sigma, x_hold.shape)
            return super(MechanizedOneVSRest, self).fit(x_hold + x_noise, y_hold)
        else:
            return train_result        

    def fit_data_split(self, x_train, y_train):
        size = len(x_train)
        hold_size, train_size = int(size  * (self.mechanism.hold_frac)), int(size  * (1.0 - self.mechanism.hold_frac))
        x_train, y_train, x_hold, y_hold = x_train[hold_size:], y_train[hold_size:], x_train[:hold_size], y_train[:hold_size]
        train_result = super(MechanizedOneVSRest, self).fit(x_train, y_train)
        train_pred = train_result.predict(x_train)
        hold_result = super(MechanizedOneVSRest, self).fit(x_hold, y_hold)
        hold_pred = hold_result.predict(x_hold)
        if abs(accuracy_score(train_pred, y_train) - accuracy_score(hold_pred, y_hold)) >= self.mechanism.noisy_thresh + np.random.laplace(self.mechanism.mu, 4 * self.mechanism.sigma):
            self.mechanism.noisy_thresh = self.mechanism.threshold + np.random.laplace(self.mechanism.mu, 2 * self.mechanism.sigma)
            x_noise =  np.random.laplace(self.mechanism.mu, self.mechanism.sigma, x_hold.shape)
            return super(MechanizedOneVSRest, self).fit(x_hold + x_noise, y_hold)
        else:
            return train_result        


    def fit(self, x_train, y_train):
        if self.mechanism.mechanism_type ==  Mechanism.MechanismType.NONE:
            print("in Baseline Mechanized One v.s. Rest")
            result = super(MechanizedOneVSRest, self).fit(x_train, y_train)
            if isinstance(result, OneVsRestClassifier):
                return self
            else:
                return result
        elif self.mechanism.mechanism_type ==  Mechanism.MechanismType.GAUSSIAN:
            print("in Gaussian Mechanized One v.s. Rest")
            x_noise = np.random.normal(0, 0.1, x_train.shape) 
            noised_x = x_train + x_noise
            result = super(MechanizedOneVSRest, self).fit(noised_x, y_train)
            if isinstance(result, OneVsRestClassifier):
                return self
            else:
                return result
        elif self.mechanism.mechanism_type ==  Mechanism.MechanismType.THRESHOLD:
            print("in Threshold Mechanized One v.s. Rest")
            return self.fit_threshold(x_train, y_train)
        elif self.mechanism.mechanism_type ==  Mechanism.MechanismType.DATASPLIT:
            print("in DataSplit Mechanized OVS")
            return self.fit_data_split(x_train, y_train)
        else:
            result = super(MechanizedOneVSRest, self).fit(x_train, y_train)
            if isinstance(result, OneVsRestClassifier):
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
        self.mechanism = Mechanism.MechanismType.NONE

    def fit(self, x_train, y_train):
        if self.mechanism.mechanism_type ==  Mechanism.MechanismType.NONE:
            result = super(MechanizedGridSearchCV, self).fit(x_train, y_train)
            if isinstance(result, GridSearchCV):
                return self
            else:
                return result
        elif self.mechanism.mechanism_type ==  Mechanism.MechanismType.GAUSSIAN:
            print("in gaussian mechanism GridSearchCV")
            x_noise = np.random.normal(0, 0.1, x_train.shape) 
            noised_x = x_train + x_noise
            
            ################ Gaussian Noise Added to Labels ################
            # y_noise = np.random.normal(0, 0.1, y_train.shape) 


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
        self.mechanism = Mechanism.MechanismType.NONE


    def fit(self, x_train, y_train):
        if self.mechanism.mechanism_type ==  Mechanism.MechanismType.NONE:
            result = super(MechanizedGaussianNB, self).fit(x_train, y_train)
            if isinstance(result, GaussianNB):
                return self
            else:
                return result
        elif self.mechanism.mechanism_type ==  Mechanism.MechanismType.GAUSSIAN:
            print("in Gaussian MechanizedGaussianNB")
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
        
        self.mechanism = Mechanism.MechanismType.NONE


    def fit(self, x_train):
        if self.mechanism.mechanism_type ==  Mechanism.MechanismType.NONE:
            result = super(MechanizedKMeans, self).fit(x_train)
            if isinstance(result, KMeans):
                return self
            else:
                return result
        elif self.mechanism.mechanism_type ==  Mechanism.MechanismType.GAUSSIAN:
            print("in gaussian MechanizedGaussianNB")
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

