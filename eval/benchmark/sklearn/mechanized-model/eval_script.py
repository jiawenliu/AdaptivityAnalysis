#!/usr/bin/env python
# coding: utf-8

# In[94]:


import pandas as pd
import numpy as np
# import matplotlib.pyplot as plt
import seaborn as sns
from sklearn import preprocessing
from sklearn.model_selection import train_test_split, KFold, GridSearchCV
from sklearn.feature_extraction.text import TfidfVectorizer


from sklearn.metrics import accuracy_score, f1_score, roc_auc_score, confusion_matrix, mean_squared_error, RocCurveDisplay
from sklearn.linear_model import LogisticRegression
from sklearn.tree import DecisionTreeClassifier
from sklearn.naive_bayes import GaussianNB
import sys
x = int(sys.argv[1])



# Basic preprocessing
def preprocess(df):
    df_new = df.copy()
    
    # Discard identifiers, style information, timestamps
    df_new = df_new[df_new.columns.difference(['image', 'style', 'reviewTime', 
                                               'reviewerID', 'asin', 'reviewerName', 'unixReviewTime'])]

    # Turn category into binary features
    for cat in df_new.category.unique():
        df_new[cat] = df_new['category'] == cat

    # Drop category column
    df_new.drop(columns=['category'], inplace=True)

    # NaN vote is 0 users found helpful
    df_new.vote.fillna(0, inplace=True)
    
    # Turn vote into binary feature
    df_new.vote = df_new.vote > 0
    # df_new.vote.clip(0, 10)
    # df_new.vote = df_new.vote / 10

    # NaN summary is empty summary
    df_new.summary.fillna('', inplace=True)

    # Turn Booleans into binary variables
    df_new.replace({False: 0, True: 1}, inplace=True)
    
    return df_new
# Remove 'overall' column and add cutoff column applying cutoff
def apply_cutoff(df, cutoff):
    df_new = df.copy()
    
    # Apply cutoff
    cut = df['overall'] > cutoff
    df_new['cutoff'] = cut

    # Drop overall and category
    df_new.drop(columns=['overall'], inplace=True)
    
    # Turn Booleans into binary variables
    df_new.replace({False: 0, True: 1}, inplace=True)
    
    return df_new
def apply_tfidf(df, review_vectorizer, summary_vectorizer):
    review_matrix = pd.DataFrame(data=review_vectorizer.transform(df.reviewText).toarray(), columns='R_' + review_vectorizer.get_feature_names_out())
    summary_matrix = pd.DataFrame(data=summary_vectorizer.transform(df.summary).toarray(), columns='S_' + summary_vectorizer.get_feature_names_out())
    df_new = pd.concat([df, review_matrix, summary_matrix], axis=1)
    df_new.drop(columns=['summary', 'reviewText'], inplace=True)
    return df_new


# Processing the data - I
# Preprocessing of training data
def load_and_process_data():
    training_df = pd.read_csv('../data/Training.csv')
    test_df = pd.read_csv('../data/Test.csv')

    proc_training_df = apply_cutoff(preprocess(training_df), 1)

    # Set cutoff to be the label; define data_x and y accordingly
    data_x = proc_training_df.drop('cutoff', axis=1)
    data_y = proc_training_df['cutoff']

    # Fit TF-IDF vectorizer for 'reviewText' and 'summary' features, creating max. 11500 features.
    r_vectorizer = TfidfVectorizer(max_features=11500, stop_words='english', ngram_range=(1, 3))
    s_vectorizer = TfidfVectorizer(max_features=11500, stop_words='english', ngram_range=(1, 3))
    r_vectorizer.fit(data_x.reviewText)
    s_vectorizer.fit(data_x.summary)

    # Apply TF-IDF vectorization 
    data_x = apply_tfidf(data_x, r_vectorizer, s_vectorizer)

    # Apply robust scaling
    scaler = preprocessing.RobustScaler()
    data_x = pd.DataFrame(scaler.fit_transform(data_x), columns=data_x.columns, index=data_x.index)

    # Let us reduce the number of features by eliminating the statistically least correlated ones.
    relcols = data_x.columns[abs(data_x.corrwith(data_y)) > 0.01]


    # We will go with these columns.
    data_x = data_x[relcols]

    return data_x, data_y

def create_splits(data_x, data_y, n_splits):
    # 5-fold cross validation
    kf = KFold(n_splits=n_splits, shuffle=True)
    splits = []
    for train_idx, val_idx in kf.split(data_x, data_y):
        # Apply split
        x_train, x_val = data_x.iloc[train_idx], data_x.iloc[val_idx]
        y_train, y_val = data_y.iloc[train_idx], data_y.iloc[val_idx]
        
        # Reset indices
        x_train.reset_index(drop=True, inplace=True)
        y_train.reset_index(drop=True, inplace=True)
        x_val.reset_index(drop=True, inplace=True)
        y_val.reset_index(drop=True, inplace=True)
        splits.append((x_train, x_val, y_train, y_val))
    return splits





X_DATA, Y_DATA = load_and_process_data()

test_splits = create_splits(X_DATA, Y_DATA, 5)

print(len(test_splits[0][1]), len(X_DATA))


# # Define and create the population data

# In[96]:


Q_MEAN = 0.5
EPOCH = 2
POPULATION_SIZE = 500000
TRAIN_DIM = 100
STEP = 1


# In[97]:


import numpy as np

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

def gen_data(n, d, seed = None):
    if seed:
        initialize_with_str_seed(seed)
    p = (1.0 + np.sqrt(max(2 * Q_MEAN - 1, 1 - 2 * Q_MEAN))) / 2 
    data = np.random.choice([-1, 1], (n, d), p=[1 -p, p])
    data_y = np.random.choice([0, 1], n, p=[1 -p, p])
    return data, data_y

def gen_valid(n, d, seed = None):
    if seed:
        initialize_with_str_seed(seed)
    
    n = int(n/10)
    
    p = (1.0 + np.sqrt(max(2 * Q_MEAN - 1, 1 - 2 * Q_MEAN))) / 2 
    data = np.random.choice([-1, 1], (n, d), p=[1 -p, p])
    data_y = np.random.choice([0, 1], n, p=[1 -p, p])
    return data, data_y



x_population, y_population = gen_data(POPULATION_SIZE, TRAIN_DIM)
x_valid, y_valid = gen_data(int(POPULATION_SIZE/500), TRAIN_DIM)


# # Define the evaluation functions

# In[98]:


import logging
logger = logging.getLogger()
logger.setLevel(logging.CRITICAL)


# In[99]:


import sys
sys.path.append("..")
from mechanism.mechanized_models import Mechanism
from mechanism.mechanized_models import MechanizedGaussianNB, MechanizedLogisticRegression, MechanizedOneVSRest, MechanizedDecisionTree
from mechanism.mechanized_models import MechanizedGridSearchCV

def hyper_parameter(splits):
    x_train, x_val, y_train, y_val = splits[0]
    estimator = MechanizedLogisticRegression(max_iter=1500)
    estimator.choose_mechanism(Mechanism.GAUSSIAN)
    gs_cls = MechanizedOneVSRest(estimator = estimator)
    gs_cls.choose_mechanism(Mechanism.GAUSSIAN)

    params_LR = {'estimator__C': np.logspace(-0.2, 0.7, num = 10)}
    gs_LR = GridSearchCV(estimator=gs_cls, param_grid=params_LR, cv = 2, verbose=2, scoring='f1_macro')
    gs_LR.fit(x_train, y_train)


BEST_C = 1.5848931924611134
'''
C=1.5848931924611134, max_iter=1500
'''




def eval_multiple_rounds(stepped_rounds, mechanism, non_adaptive_num):
    generalization_error_list = []
    for r in stepped_rounds:
        estimator = MechanizedLogisticRegression(C = BEST_C, max_iter = r, mechanism = mechanism, solver = 'sag')
        generalization_error_list.append(eval(non_adaptive_num, estimator, mechanism))
    return generalization_error_list



# # Evaluation the O(1) adaptivity program

def eval_const(round, train_size, mechanism):
    # f1_scores, acc_scores, models = [], [], [], [], []
    x_train, y_train = x_population[:train_size], y_population[:train_size]    
    model = MechanizedLogisticRegression(C = BEST_C, max_iter = round, mechanism = mechanism)
    # print(y_train)
    # lab_enc = preprocessing.LabelEncoder()
    # encoded = lab_enc.fit_transform(y_train) 
    # print(encoded)
    model.fit(x_train, y_train)
    # Predict
    y_pred = model.predict(x_valid)
    
    mean = np.mean(y_train)
    ## Here it returns MSE, if we want to have RMSE, we need to give mean_squared_error(y_valid, y_pred, squared = false)
    rmse = mean_squared_error(y_valid, y_pred, squared=False)
    # print(rmse)
    # print(mean)
    nrmse = rmse/mean
    return rmse

def eval_const_rounds(round, mechanism, stepped_non_adaptive_num):
    generalization_error_list = []
    for train_size in stepped_non_adaptive_num:
        generalization_error_list.append(eval_const(round, train_size, mechanism))
    # print(generalization_error_list)
    return generalization_error_list

def eval_const_dt(round, train_size, mechanism):
    # f1_scores, acc_scores, models = [], [], [], [], []
    x_train, y_train = x_population[:train_size], y_population[:train_size]    
    model = MechanizedDecisionTree( mechanism = mechanism)
    # print(y_train)
    # lab_enc = preprocessing.LabelEncoder()
    # encoded = lab_enc.fit_transform(y_train) 
    # print(encoded)
    model.fit(x_train, y_train)
    # Predict
    y_pred = model.predict(x_valid)
    
    mean = np.mean(y_train)
    ## Here it returns MSE, if we want to have RMSE, we need to give mean_squared_error(y_valid, y_pred, squared = false)
    rmse = mean_squared_error(y_valid, y_pred, squared=False)
    # print(rmse)
    # print(mean)
    nrmse = rmse/mean
    return rmse

def eval_const_rounds_dt(round, mechanism, stepped_non_adaptive_num):
    generalization_error_list = []
    for train_size in stepped_non_adaptive_num:
        generalization_error_list.append(eval_const_dt(round, train_size, mechanism))
    # print(generalization_error_list)
    return generalization_error_list


stepped_non_adaptive_num = [1000]
round = x
# stepped_non_adaptive_num = range(10, 100, 20)
# stepped_non_adaptive_num = range(1000, 1010, 10)


baseline_generalization_error_list_lr = eval_const_rounds(round, Mechanism(mechanism_type = Mechanism.MechanismType.NONE), stepped_non_adaptive_num)
# baseline_generalization_error_list = [0.47190818773552584, 0.473107228502912, 0.473792394655704, 0.47070914696813976, 0.4659129838985954, 0.4720794792737239, 0.47447756080849607, 0.473107228502912, 0.4775608084960603, 0.47619047619047616, 0.4712230215827338, 0.47447756080849607, 0.47567660157588215, 0.473792394655704, 0.47533401849948614]
gaussian_generalization_error_list_lr = eval_const_rounds(round, Mechanism(mechanism_type = Mechanism.MechanismType.GAUSSIAN, sigma = 0.03), stepped_non_adaptive_num)
# gaussian_generalization_error_list = [0.3984241178485783, 0.4035628639945187, 0.39482699554642003, 0.3977389516957862, 0.3977389516957862, 0.3917437478588558, 0.3975676601575882, 0.39585474477560806, 0.38866050017129156, 0.3970537855429942, 0.39568345323741005, 0.3857485440219253, 0.38506337786913325, 0.39157245632065774, 0.3871188763275094]

threshold_generalization_error_list_lr = eval_const_rounds(round, Mechanism(mechanism_type = Mechanism.MechanismType.THRESHOLD, sigma = 0.05, hold_frac = 0.7, threshold = 0.8), stepped_non_adaptive_num)
# gaussian_generalization_error_list = [0.3984241178485783, 0.4035628639945187, 0.39482699554642003, 0.3977389516957862, 0.3977389516957862, 0.3917437478588558, 0.3975676601575882, 0.39585474477560806, 0.38866050017129156, 0.3970537855429942, 0.39568345323741005, 0.3857485440219253, 0.38506337786913325, 0.39157245632065774, 0.3871188763275094]


datasplit_generalization_error_list_lr = eval_const_rounds(round, Mechanism(mechanism_type = Mechanism.MechanismType.DATASPLIT), stepped_non_adaptive_num)


# In[186]:


baseline_generalization_error_list_dt = eval_const_rounds_dt(round, Mechanism(mechanism_type = Mechanism.MechanismType.NONE), stepped_non_adaptive_num)
# baseline_generalization_error_list = [0.47190818773552584, 0.473107228502912, 0.473792394655704, 0.47070914696813976, 0.4659129838985954, 0.4720794792737239, 0.47447756080849607, 0.473107228502912, 0.4775608084960603, 0.47619047619047616, 0.4712230215827338, 0.47447756080849607, 0.47567660157588215, 0.473792394655704, 0.47533401849948614]

gaussian_generalization_error_list_dt = eval_const_rounds_dt(round, Mechanism(mechanism_type = Mechanism.MechanismType.GAUSSIAN, sigma = 0.03), stepped_non_adaptive_num)
# gaussian_generalization_error_list = [0.3984241178485783, 0.4035628639945187, 0.39482699554642003, 0.3977389516957862, 0.3977389516957862, 0.3917437478588558, 0.3975676601575882, 0.39585474477560806, 0.38866050017129156, 0.3970537855429942, 0.39568345323741005, 0.3857485440219253, 0.38506337786913325, 0.39157245632065774, 0.3871188763275094]

threshold_generalization_error_list_dt = eval_const_rounds_dt(round, Mechanism(mechanism_type = Mechanism.MechanismType.THRESHOLD, sigma = 0.05, hold_frac = 0.7, threshold = 0.8), stepped_non_adaptive_num)
# gaussian_generalization_error_list = [0.3984241178485783, 0.4035628639945187, 0.39482699554642003, 0.3977389516957862, 0.3977389516957862, 0.3917437478588558, 0.3975676601575882, 0.39585474477560806, 0.38866050017129156, 0.3970537855429942, 0.39568345323741005, 0.3857485440219253, 0.38506337786913325, 0.39157245632065774, 0.3871188763275094]


datasplit_generalization_error_list_dt = eval_const_rounds_dt(round, Mechanism(mechanism_type = Mechanism.MechanismType.DATASPLIT), stepped_non_adaptive_num)







# # # Evaluate the O(n*m) Adaptivity Program


def eval_nm(round, train_size, mechanism):
    # f1_scores, acc_scores, models = [], [], [], [], []
    x_train, y_train = x_population[:train_size], y_population[:train_size]
    
    estimator = MechanizedLogisticRegression(C = BEST_C, max_iter = round, mechanism = mechanism, solver = 'lbfgs', random_state = np.random.randint(1000000))
    model = MechanizedOneVSRest(estimator, mechanism = mechanism)

    model.fit(x_train, y_train)
    # Predict
    y_pred = model.predict(x_valid)
    mean = np.mean(y_train)
    ## Here it returns MSE, if we want to have RMSE, we need to give mean_squared_error(y_valid, y_pred, squared = false)
    rmse = mean_squared_error(y_valid, y_pred, squared=False)
    # print(rmse)
    # print(mean)
    nrmse = rmse/mean
    return rmse
    


def eval_nm_rounds(stepped_rounds, mechanism, non_adaptive_num):
    generalization_error_list = []
    for r in stepped_rounds:
        # estimator = MechanizedLogisticRegression(C = BEST_C, max_iter = r, mechanism = mechanism, solver = 'sag')
        generalization_error_list.append(eval_nm(r, non_adaptive_num, mechanism))
    return generalization_error_list

def eval_nm_dt(round, train_size, mechanism):
    # f1_scores, acc_scores, models = [], [], [], [], []
    x_train, y_train = x_population[:train_size], y_population[:train_size]
    
    estimator = MechanizedDecisionTree( mechanism = mechanism,)
    model = MechanizedOneVSRest(estimator, mechanism = mechanism)

    model.fit(x_train, y_train)
    # Predict
    y_pred = model.predict(x_valid)
    mean = np.mean(y_train)
    ## Here it returns MSE, if we want to have RMSE, we need to give mean_squared_error(y_valid, y_pred, squared = false)
    rmse = mean_squared_error(y_valid, y_pred, squared=False)
    # print(rmse)
    # print(mean)
    nrmse = rmse/mean
    return rmse
    


def eval_nm_rounds_dt(stepped_rounds, mechanism, non_adaptive_num):
    generalization_error_list = []
    for r in stepped_rounds:
        # estimator = MechanizedLogisticRegression(C = BEST_C, max_iter = r, mechanism = mechanism, solver = 'sag')
        generalization_error_list.append(eval_nm_dt(r, non_adaptive_num, mechanism))
    return generalization_error_list


# # In[206]:


stepped_rounds = [x] #k
non_adaptive_num = 1000 




baseline_generalization_error_list_lrovr = eval_nm_rounds(stepped_rounds, Mechanism(mechanism_type = Mechanism.MechanismType.NONE), non_adaptive_num)
# print((baseline_generalization_error_list))
gaussian_generalization_error_list_lrovr = eval_nm_rounds(stepped_rounds, Mechanism(mechanism_type = Mechanism.MechanismType.GAUSSIAN, sigma = 0.08), non_adaptive_num)
# print(gaussian_generalization_error_list) 
# = [0.3984241178485783, 0.4035628639945187, 0.39482699554642003, 0.3977389516957862, 0.3977389516957862, 0.3917437478588558, 0.3975676601575882, 0.39585474477560806, 0.38866050017129156, 0.3970537855429942, 0.39568345323741005, 0.3857485440219253, 0.38506337786913325, 0.39157245632065774, 0.3871188763275094]
threshold_generalization_error_list_lrovr = eval_nm_rounds(stepped_rounds, Mechanism(mechanism_type = Mechanism.MechanismType.THRESHOLD, sigma = 0.08, hold_frac = 0.7, threshold = 0.9), non_adaptive_num)
# gaussian_generalization_error_list = [0.3984241178485783, 0.4035628639945187, 0.39482699554642003, 0.3977389516957862, 0.3977389516957862, 0.3917437478588558, 0.3975676601575882, 0.39585474477560806, 0.38866050017129156, 0.3970537855429942, 0.39568345323741005, 0.3857485440219253, 0.38506337786913325, 0.39157245632065774, 0.3871188763275094]

datasplit_generalization_error_list_lrovr = eval_nm_rounds(stepped_rounds, Mechanism(mechanism_type = Mechanism.MechanismType.DATASPLIT), non_adaptive_num)


baseline_generalization_error_list_dtovr = eval_nm_rounds_dt(stepped_rounds, Mechanism(mechanism_type = Mechanism.MechanismType.NONE), non_adaptive_num)

gaussian_generalization_error_list_dtovr = eval_nm_rounds_dt(stepped_rounds, Mechanism(mechanism_type = Mechanism.MechanismType.GAUSSIAN, sigma = 0.08), non_adaptive_num)

# = [0.3984241178485783, 0.4035628639945187, 0.39482699554642003, 0.3977389516957862, 0.3977389516957862, 0.3917437478588558, 0.3975676601575882, 0.39585474477560806, 0.38866050017129156, 0.3970537855429942, 0.39568345323741005, 0.3857485440219253, 0.38506337786913325, 0.39157245632065774, 0.3871188763275094]
threshold_generalization_error_list_dtovr = eval_nm_rounds_dt(stepped_rounds, Mechanism(mechanism_type = Mechanism.MechanismType.THRESHOLD, sigma = 0.08, hold_frac = 0.7, threshold = 0.9), non_adaptive_num)
# gaussian_generalization_error_list = [0.3984241178485783, 0.4035628639945187, 0.39482699554642003, 0.3977389516957862, 0.3977389516957862, 0.3917437478588558, 0.3975676601575882, 0.39585474477560806, 0.38866050017129156, 0.3970537855429942, 0.39568345323741005, 0.3857485440219253, 0.38506337786913325, 0.39157245632065774, 0.3871188763275094]

datasplit_generalization_error_list_dtovr = eval_nm_rounds_dt(stepped_rounds, Mechanism(mechanism_type = Mechanism.MechanismType.DATASPLIT), non_adaptive_num)



print("THE OUTPUT OF DT")
print(np.mean(baseline_generalization_error_list_dt))
print(np.mean(datasplit_generalization_error_list_dt))
print(np.mean(gaussian_generalization_error_list_dt))
print(np.mean(threshold_generalization_error_list_dt))


# The output of LR
print("THE OUTPUT OF LR")
print(np.mean(baseline_generalization_error_list_lr))
print(np.mean(datasplit_generalization_error_list_lr))
print(np.mean(gaussian_generalization_error_list_lr))
print(np.mean(threshold_generalization_error_list_lr))


# #The result of DTOVR and LROVR


print("The reusult of LROVR")
print(np.mean(baseline_generalization_error_list_lrovr))
print(np.mean(datasplit_generalization_error_list_lrovr))
print(np.mean(gaussian_generalization_error_list_lrovr))
print(np.mean(threshold_generalization_error_list_lrovr))
print("The reusult of DTOVR")
print(np.mean(baseline_generalization_error_list_dtovr))
print(np.mean(datasplit_generalization_error_list_dtovr))
print(np.mean(gaussian_generalization_error_list_dtovr))
print(np.mean(threshold_generalization_error_list_dtovr))

# # plt.figure()
# # x_range = stepped_rounds
# # plot_error(x_range, np.sqrt(baseline_generalization_error_list), "Baseline")
# # plot_error(x_range, np.sqrt(gaussian_generalization_error_list), "Gaussian - Adaptfun")
# # plot_error(x_range, np.sqrt(threshold_generalization_error_list), "Threshold")
# # plt.show()


# # # Evaluate the O(n) Adaptivity Program

# # In[210]:


# def eval_n(round, train_size, mechanism):
#     # f1_scores, acc_scores, models = [], [], [], [], []
#     x_train, y_train = x_population[:train_size], y_population[:train_size]    
#     model = MechanizedLogisticRegression(C = BEST_C, max_iter = round, mechanism = mechanism)

#     model.fit(x_train, y_train)
#     # Predict
#     y_pred = model.predict(x_valid)
#     mean = np.mean(y_train)
#     ## Here it returns MSE, if we want to have RMSE, we need to give mean_squared_error(y_valid, y_pred, squared = false)
#     rmse = mean_squared_error(y_valid, y_pred, squared=False)
#     # print(rmse)
#     # print(mean)
#     nrmse = rmse/mean
#     return nrmse

# def eval_multiple_rounds(stepped_rounds, mechanism, non_adaptive_num):
#     generalization_error_list = []
#     for r in stepped_rounds:
#         generalization_error_list.append(eval_n(r, non_adaptive_num, mechanism))
#     return generalization_error_list


# # In[211]:


# stepped_rounds = [10]
# non_adaptive_num = 1000



# # In[ ]:


# baseline_generalization_error_list = eval_multiple_rounds(stepped_rounds, Mechanism(mechanism_type = Mechanism.MechanismType.NONE), non_adaptive_num)
# gaussian_generalization_error_list = eval_multiple_rounds(stepped_rounds, Mechanism(mechanism_type = Mechanism.MechanismType.GAUSSIAN, sigma = 0.1), non_adaptive_num)
# threshold_generalization_error_list = eval_multiple_rounds(stepped_rounds, Mechanism(mechanism_type = Mechanism.MechanismType.THRESHOLD, sigma = 0.08, hold_frac = 0.7, threshold = 0.8), non_adaptive_num)
# datasplit_generalization_error_list = eval_multiple_rounds(stepped_rounds, Mechanism(mechanism_type = Mechanism.MechanismType.DATASPLIT), non_adaptive_num)


# # In[213]:


# print(gaussian_generalization_error_list)


# # In[214]:


# print(np.mean(baseline_generalization_error_list))
# print(np.mean(datasplit_generalization_error_list))
# print(np.mean(gaussian_generalization_error_list))
# print(np.mean(threshold_generalization_error_list))


# # In[ ]:


# # threshold_generalization_error_list = eval_multiple_rounds(stepped_rounds, Mechanism(mechanism_type = Mechanism.MechanismType.THRESHOLD, sigma = 0.08, hold_frac = 0.7, threshold = 0.8), non_adaptive_num)
# # gaussian_generalization_error_list = [0.3984241178485783, 0.4035628639945187, 0.39482699554642003, 0.3977389516957862, 0.3977389516957862, 0.3917437478588558, 0.3975676601575882, 0.39585474477560806, 0.38866050017129156, 0.3970537855429942, 0.39568345323741005, 0.3857485440219253, 0.38506337786913325, 0.39157245632065774, 0.3871188763275094]



# # In[ ]:


# # plt.figure()
# # x_range = stepped_rounds
# # plot_error(x_range, baseline_generalization_error_list, "Baseline")
# # plot_error(x_range, gaussian_generalization_error_list, "Gaussian")
# # plot_error(x_range, threshold_generalization_error_list, "Threshold - Adaptfun")
# # plt.show()


# # In[79]:


# stepped_non_adaptive_num = range(100, 10000, 100)
# round = 10


# # In[ ]:


# baseline_generalization_error_list = eval_const_rounds(round, Mechanism(mechanism_type = Mechanism.MechanismType.NONE), stepped_non_adaptive_num)
# # baseline_generalization_error_list = [0.47190818773552584, 0.473107228502912, 0.473792394655704, 0.47070914696813976, 0.4659129838985954, 0.4720794792737239, 0.47447756080849607, 0.473107228502912, 0.4775608084960603, 0.47619047619047616, 0.4712230215827338, 0.47447756080849607, 0.47567660157588215, 0.473792394655704, 0.47533401849948614]
# print(baseline_generalization_error_list)
# threshold_generalization_error_list = eval_const_rounds(round, Mechanism(mechanism_type = Mechanism.MechanismType.THRESHOLD, sigma = 0.08, hold_frac = 0.7, threshold = 0.9), stepped_non_adaptive_num)
# # gaussian_generalization_error_list = [0.3984241178485783, 0.4035628639945187, 0.39482699554642003, 0.3977389516957862, 0.3977389516957862, 0.3917437478588558, 0.3975676601575882, 0.39585474477560806, 0.38866050017129156, 0.3970537855429942, 0.39568345323741005, 0.3857485440219253, 0.38506337786913325, 0.39157245632065774, 0.3871188763275094]

# print(threshold_generalization_error_list)
# gaussian_generalization_error_list = eval_const_rounds(round, Mechanism(mechanism_type = Mechanism.MechanismType.GAUSSIAN, sigma = 0.1), stepped_non_adaptive_num)
# # gaussian_generalization_error_list = [0.3984241178485783, 0.4035628639945187, 0.39482699554642003, 0.3977389516957862, 0.3977389516957862, 0.3917437478588558, 0.3975676601575882, 0.39585474477560806, 0.38866050017129156, 0.3970537855429942, 0.39568345323741005, 0.3857485440219253, 0.38506337786913325, 0.39157245632065774, 0.3871188763275094]

# print(gaussian_generalization_error_list)
# datasplit_generalization_error_list = eval_const_rounds(round, Mechanism(mechanism_type = Mechanism.MechanismType.DATASPLIT), stepped_non_adaptive_num)


# # In[84]:


# print(np.mean(baseline_generalization_error_list))
# print(np.mean(datasplit_generalization_error_list))
# print(np.mean(gaussian_generalization_error_list))
# print(np.mean(threshold_generalization_error_list))


# # In[ ]:





# # In[ ]:


# # plt.figure()
# # x_range = stepped_non_adaptive_num
# # print(x_range)
# # print(len(baseline_generalization_error_list))

# # plot_error(x_range, (baseline_generalization_error_list), "Baseline")
# # plot_error(x_range, (gaussian_generalization_error_list), "Gaussian - Adaptfun")
# # plot_error(x_range, (threshold_generalization_error_list), "Threshold")
# # plt.show()

