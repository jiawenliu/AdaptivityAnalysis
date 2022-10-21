import pandas as pd
import numpy as np
import matplotlib.pyplot as plt
from sklearn.linear_model import LogisticRegression
from itertools import combinations
from sklearn.base import clone
from sklearn.metrics import accuracy_score
 
class SequentialForwardSelection():
     
    '''
    Instantiate with Estimator and given number of features
    '''
    def __init__(self, estimator, k_features):
        self.estimator = clone(estimator)
        self.k_features = k_features
         
    '''
    X_train - Training data Pandas dataframe
    X_test - Test data Pandas dataframe
    y_train - Training label Pandas dataframe
    y_test - Test data Pandas dataframe
    '''  
    def fit(self, X_train, X_test, y_train, y_test):
        max_indices = tuple(range(X_train.shape[1]))
        total_features_count = len(max_indices)
        self.subsets_ = []
        self.scores_ = []
        self.indices_ = []
        '''
        Iterate through the feature space to find the first feature
        which gives the maximum model performance
        '''
        scores = []
        subsets = []
        for p in combinations(max_indices, r=1):
                score = self._calc_score(X_train.values, X_test.values, y_train.values, y_test.values, p)
                scores.append(score)
                subsets.append(p)
        #
        # Find the single feature having best score
        #
        best_score_index = np.argmax(scores)
        self.scores_.append(scores[best_score_index])
        self.indices_ = list(subsets[best_score_index])
        self.subsets_.append(self.indices_)
         
        #
        # Add a feature one by one until k_features is reached
        #
        dim = 1
        while dim < self.k_features:
            scores = []
            subsets = []
            current_feature = dim
            '''
            Add the remaining features one-by-one from the remaining feature set
            Calculate the score for every feature combinations
            '''
            idx = 0
            while idx < total_features_count:
                if idx not in self.indices_:
                    indices = list(self.indices_)
                    indices.append(idx)
                    score = self._calc_score(X_train.values, X_test.values, y_train.values, y_test.values,indices)
                    scores.append(score)
                    subsets.append(indices)
                idx += 1
             
            #
            # Get the index of best score
            #
            best_score_index = np.argmax(scores)
            #
            # Record the best score
            #
            self.scores_.append(scores[best_score_index])
            #
            # Get the indices of features which gave best score
            #
            self.indices_ = list(subsets[best_score_index])
            #
            # Record the indices of features for best score
            #
            self.subsets_.append(self.indices_)
             
            dim += 1
             
        self.k_score_ = self.scores_[-1]
     
    '''
    Transform training, test data set to the data set
    havng features which gave best score
    '''
    def transform(self, X):
        return X.values[:, self.indices_]
     
    '''
    Train models with specific set of features
    indices - indices of features
    '''
    def _calc_score(self, X_train, X_test, y_train, y_test, indices):
        self.estimator.fit(X_train[:, indices], y_train.ravel())
        y_pred = self.estimator.predict(X_test[:, indices])
        score = accuracy_score(y_test, y_pred)
        return score









#
# Instantiate the estimator - LogisticRegression
#
lr = LogisticRegression(C=1.0, random_state=1)
#
# Number of features
#
k_features = 5
#
# Instantiate SequentialBackwardSearch
#
sfs = SequentialForwardSelection(lr, k_features)
#
# Fit the data to determine the k_features which give the
# most optimal model performance
#
sfs.fit(X_train, X_test, y_train, y_test)
#
# Transform the training data set to dataset having k_features
# giving most optimal model performance
#
X_train_sfs = sfs.transform(X_train)
#
# Transform the test data set to dataset having k_features
#
X_test_sfs = sfs.transform(X_test)





k_features = [len(k) for k in sfs.subsets_]
plt.plot(k_features, sfs.scores_, marker='o')
plt.ylim([0.7, 1.02])
plt.ylabel('Accuracy')
plt.xlabel('Number of features')
plt.grid()
plt.tight_layout()
plt.show()


