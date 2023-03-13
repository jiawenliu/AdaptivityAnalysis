import pandas as pd
import numpy as np
import matplotlib.pyplot as plt
import seaborn as sns
from sklearn import preprocessing
from sklearn.model_selection import train_test_split, KFold, GridSearchCV
from sklearn.feature_extraction.text import TfidfVectorizer


from sklearn.metrics import accuracy_score, f1_score, roc_auc_score, confusion_matrix, RocCurveDisplay
from sklearn.linear_model import LogisticRegression
from sklearn.tree import DecisionTreeClassifier
from sklearn.naive_bayes import GaussianNB




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
    kf = KFold(n_splits=n_splits, shuffle=True, random_state=42)
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

## Mechanized Decision Tree Classifier
# Hyperparameter selection on one split
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

X_DATA, Y_DATA = load_and_process_data()

# print(len(X_DATA))

def eval(estimator, mechanism, splits):
    # f1_scores, acc_scores, models = [], [], [], [], []
    x_train, x_val, y_train, y_val = splits[0]
    estimator = estimator
    
    model = MechanizedOneVSRest(estimator, mechanism = mechanism)
    # model.choose_mechanism(mechanism)

    model.fit(x_train, y_train)
    # Predict
    y_pred = model.predict(x_val)

    return accuracy_score(y_val, y_pred)


def eval_multiple_rounds(stepped_rounds, mechanism, non_adaptive_num):
    splits = create_splits(X_DATA, Y_DATA, non_adaptive_num)
    generalization_error_list = []
    for r in stepped_rounds:
        estimator = MechanizedLogisticRegression(C = BEST_C, max_iter = r, mechanism = mechanism)
        generalization_error_list.append(eval(estimator, mechanism, splits))
    return generalization_error_list

def eval_const_rounds(round, mechanism, stepped_non_adaptive_num):
    generalization_error_list = []
    for n_splits in stepped_non_adaptive_num:
        splits = create_splits(X_DATA, Y_DATA, n_splits)
        estimator = MechanizedLogisticRegression(C = BEST_C, max_iter = round, mechanism = mechanism)
        generalization_error_list.append(eval(estimator, mechanism, splits))
    return generalization_error_list

stepped_non_adaptive_num = range(2, 4, 1)
round = 1
mechanism = Mechanism(mechanism_type = Mechanism.MechanismType.GAUSSIAN, sigma = 0.03)
eval_const_rounds(round, mechanism, stepped_non_adaptive_num)

baseline_generalization_error_list = eval_const_rounds(round, Mechanism(mechanism_type = Mechanism.MechanismType.NONE), stepped_non_adaptive_num)
# baseline_generalization_error_list = [0.47190818773552584, 0.473107228502912, 0.473792394655704, 0.47070914696813976, 0.4659129838985954, 0.4720794792737239, 0.47447756080849607, 0.473107228502912, 0.4775608084960603, 0.47619047619047616, 0.4712230215827338, 0.47447756080849607, 0.47567660157588215, 0.473792394655704, 0.47533401849948614]

gaussian_generalization_error_list = eval_const_rounds(round, Mechanism(mechanism_type = Mechanism.MechanismType.GAUSSIAN, sigma = 0.1), stepped_non_adaptive_num)
# gaussian_generalization_error_list = [0.3984241178485783, 0.4035628639945187, 0.39482699554642003, 0.3977389516957862, 0.3977389516957862, 0.3917437478588558, 0.3975676601575882, 0.39585474477560806, 0.38866050017129156, 0.3970537855429942, 0.39568345323741005, 0.3857485440219253, 0.38506337786913325, 0.39157245632065774, 0.3871188763275094]

def plot_error(rounds, generalization_error, mechanism):
    plt.plot(rounds, generalization_error, label = mechanism)
    plt.xlabel("Queries")
    plt.ylabel("RMSE (Generalization Error) for adaptive queries")
    plt.legend()
    plt.grid()

    
plt.figure()
x_range = [len(X_DATA)/ step for step in stepped_non_adaptive_num]
plot_error(x_range, baseline_generalization_error_list, "Baseline")
plot_error(x_range, gaussian_generalization_error_list, "Gaussian")
# plot_error(stepped_rounds, threshold_generalization_error_list, "Threshold")
plt.show()