
import math
import numpy as np
import matplotlib.pyplot as plt

from enum import Enum
import sys
sys.path.append("..")

import cw_funcs as cw
import helper_funcs as hf
import strategies as stg
import mechanisms as mech

# strategy = stg.Strategy(n,  ada_freq = {"method": "additive", "method_param": q_adapt}, q_max=q_max)
DATA_SIZE = 1000
CARDINALITY = 1000
MAX_QUERY_NUM = 1000
MAX_EPOCH = 100

def n_dim_pairwise(delta, strategy, mechanism, epoch = MAX_EPOCH):
    l, queried_set = 0, []
    q = strategy.next_query()
    a = -1
    ans_list, true_ans_list = [], []
    while q and l < epoch:
        delta_l = np.sqrt((l + 1) * np.log(2 / delta) / (2**l))
        if q is None:
            break
        r = mechanism.get_answer(q["query"])
        if r[0]["answer"] is not None:
            queried_set.append(r[0]["answer"] * 2.0 - 1)
            if "empirical_answer" in r[0].keys():
                pre_ans = [{"answer": 1.0 if (np.sign(np.mean(queried_set)) == np.sign(r[0]["empirical_answer"])) else 0.0}]
                true_ans_list.append(strategy.true_ans_list[-1])           
            else:
                pre_ans = [{"answer": 1.0 if (np.sign(np.mean(queried_set)) == np.sign(r[0]["answer"])) else 0.0}]
            q = strategy.next_query(pre_ans)
        else:
            q = None
            break
        ans_list.append(strategy.mech_ans_list[-1])
        l = l + 1
        a = abs(np.sum(np.sign(queried_set))) - delta_l
    return true_ans_list, ans_list




def eval_n_dim_pairwise(delta, n = DATA_SIZE, cardinality = CARDINALITY, q_max = MAX_QUERY_NUM, mechanism = mech.Mechanism()):
    strategy = stg.Strategy(n, q_mean = 0.5, ada_freq = {"method": "n_dim_pairwise", "method_param": q_max}, q_max = q_max, cardinality = cardinality)
    mechanism.reset()
    mechanism.add_data({'data': strategy.gen_data()})

    true_ans_list, mech_ans_list = n_dim_pairwise(delta, strategy, mechanism)
    q_done = min(len(strategy.true_ans_list), len(strategy.mech_ans_list))
    mse = np.square(np.subtract(strategy.true_ans_list[:q_done], strategy.mech_ans_list[:q_done]))

    return np.sqrt(mse)

n = 1000
dimension = 100
q_max = 1000
runs = 10

stepped_q_max = range(q_max/2, q_max, 10)

beta, tau = 0.05, 1.0
sigma = 3.5
hold_frac, threshold, check_data_frac = 0.7, 0.05, 0.05

repeated_query_sub_delta = 0.1

Baseline = mech.Mechanism()
Baseline.add_params(beta=beta, tau=tau, check_for_width=None)
# Baseline_rmse = [eval_n_dim_pairwise(repeated_query_sub_delta, n, dimension, q_max, Baseline).mean() for q_max in stepped_q_max]
Baseline_rmse = eval_n_dim_pairwise(repeated_query_sub_delta, n, dimension, q_max, Baseline)

DataSplit = mech.Mechanism()
DataSplit.add_params(beta=beta, tau=tau)
DataSplit_rmse = eval_n_dim_pairwise(repeated_query_sub_delta, n, dimension, q_max, DataSplit)

Thresh = mech.Thresholdout_Mechanism(hold_frac=hold_frac, threshold=threshold, sigma=sigma)
Thresh.add_params(beta=beta, tau=tau, check_for_width=None)
# Thresh_rmse = [eval_n_dim_pairwise(repeated_query_sub_delta, n, dimension, q_max, Thresh).mean() for q_max in stepped_q_max]
Thresh_rmse = eval_n_dim_pairwise(repeated_query_sub_delta, n, dimension, q_max, Thresh)


Gauss = mech.Gaussian_Mechanism(sigma=sigma)
Gauss.add_params(beta=beta, tau=tau, check_for_width=None)
# Gauss_rmse = [eval_n_dim_pairwise(repeated_query_sub_delta, n, dimension, q_max, Gauss).mean() for q_max in stepped_q_max]
Gauss_rmse = eval_n_dim_pairwise(repeated_query_sub_delta, n, dimension, q_max, Gauss)

print(Baseline_rmse, DataSplit_rmse, Gauss_rmse, Thresh_rmse)

'''
('Splitted Sample Size:', 10)
(array([0.5, 0.5, 0.5, 0.5, 0.5, 0.5, 0.5, 0.5, 0.5, 0.5, 0.5, 0.5, 0.5,
       0.5, 0.5, 0.5, 0.5, 0.5, 0.5, 0.5, 0.5, 0.5, 0.5, 0.5, 0.5, 0.5,
       0.5, 0.5, 0.5, 0.5, 0.5, 0.5, 0.5, 0.5, 0.5, 0.5, 0.5, 0.5, 0.5,
       0.5, 0.5, 0.5, 0.5, 0.5, 0.5, 0.5, 0.5, 0.5, 0.5, 0.5, 0.5, 0.5,
       0.5, 0.5, 0.5, 0.5, 0.5, 0.5, 0.5, 0.5, 0.5, 0.5, 0.5, 0.5, 0.5,
       0.5, 0.5, 0.5, 0.5, 0.5, 0.5, 0.5, 0.5, 0.5, 0.5, 0.5, 0.5, 0.5,
       0.5, 0.5, 0.5, 0.5, 0.5, 0.5, 0.5, 0.5, 0.5, 0.5, 0.5, 0.5, 0.5,
       0.5, 0.5, 0.5, 0.5, 0.5, 0.5, 0.5, 0.5, 0.5]), array([0.5, 0.5, 0.5, 0.5, 0.5, 0.5, 0.5, 0.5, 0.5, 0.5, 0.5, 0.5, 0.5,
       0.5, 0.5, 0.5, 0.5, 0.5, 0.5, 0.5, 0.5, 0.5, 0.5, 0.5, 0.5, 0.5,
       0.5, 0.5, 0.5, 0.5, 0.5, 0.5, 0.5, 0.5, 0.5, 0.5, 0.5, 0.5, 0.5,
       0.5, 0.5, 0.5, 0.5, 0.5, 0.5, 0.5, 0.5, 0.5, 0.5, 0.5, 0.5, 0.5,
       0.5, 0.5, 0.5, 0.5, 0.5, 0.5, 0.5, 0.5, 0.5, 0.5, 0.5, 0.5, 0.5,
       0.5, 0.5, 0.5, 0.5, 0.5, 0.5, 0.5, 0.5, 0.5, 0.5, 0.5, 0.5, 0.5,
       0.5, 0.5, 0.5, 0.5, 0.5, 0.5, 0.5, 0.5, 0.5, 0.5, 0.5, 0.5, 0.5,
       0.5, 0.5, 0.5, 0.5, 0.5, 0.5, 0.5, 0.5, 0.5]), array([0.5       , 0.5       , 0.5       , 0.5       , 0.5       ,
       0.35714286, 0.26785714, 0.20833333, 0.16666667, 0.13636364,
       0.11363636, 0.09615385, 0.08241758, 0.07142857, 0.0625    ,
       0.05514706, 0.04901961, 0.04385965, 0.03947368, 0.03571429,
       0.03246753, 0.02964427, 0.02717391, 0.025     , 0.02307692,
       0.02136752, 0.01984127, 0.01847291, 0.01724138, 0.01612903,
       0.04637097, 0.04356061, 0.04099822, 0.03865546, 0.03650794,
       0.06156156, 0.08463727, 0.10593792, 0.10064103, 0.12012195,
       0.13821138, 0.15503876, 0.17071882, 0.18535354, 0.19903382,
       0.21184089, 0.22384752, 0.23511905, 0.24571429, 0.25568627,
       0.26508296, 0.27394775, 0.28232006, 0.29023569, 0.29772727,
       0.30482456, 0.31155475, 0.31794272, 0.3240113 , 0.32978142,
       0.33527234, 0.34050179, 0.34548611, 0.35024038, 0.35477855,
       0.35911352, 0.36325724, 0.3672208 , 0.37101449, 0.37464789,
       0.37812989, 0.3814688 , 0.38467234, 0.38774775, 0.39070175,
       0.39354067, 0.3962704 , 0.39889646, 0.40142405, 0.40385802,
       0.40620295, 0.40846312, 0.41064257, 0.4127451 , 0.41477428,
       0.41673349, 0.41862591, 0.42045455, 0.42222222, 0.42393162,
       0.42558528, 0.4271856 , 0.42873484, 0.43023516, 0.4316886 ,
       0.43309708, 0.43446244, 0.43578644, 0.43707071, 0.43831683]), array([0.        , 0.        , 0.        , 0.        , 0.        ,
       0.        , 0.        , 0.        , 0.        , 0.        ,
       0.08333333, 0.1474359 , 0.1978022 , 0.23809524, 0.27083333,
       0.29779412, 0.32026144, 0.33918129, 0.35526316, 0.36904762,
       0.38095238, 0.39130435, 0.40036232, 0.40833333, 0.41538462,
       0.42165242, 0.42724868, 0.43226601, 0.43678161, 0.44086022,
       0.44455645, 0.44791667, 0.45098039, 0.45378151, 0.45634921,
       0.45870871, 0.46088193, 0.46288799, 0.46474359, 0.46646341,
       0.46806039, 0.46954596, 0.47093023, 0.47222222, 0.47342995,
       0.47456059, 0.47562057, 0.47661565, 0.47755102, 0.47843137,
       0.47926094, 0.48004354, 0.48078267, 0.48148148, 0.48214286,
       0.48276942, 0.48336358, 0.48392753, 0.48446328, 0.48497268,
       0.48545743, 0.4859191 , 0.48635913, 0.48677885, 0.48717949,
       0.48756219, 0.48792801, 0.48827792, 0.48861284, 0.4889336 ,
       0.489241  , 0.48953577, 0.48981859, 0.49009009, 0.49035088,
       0.4906015 , 0.49084249, 0.49107433, 0.49129747, 0.49151235,
       0.49171936, 0.4919189 , 0.4921113 , 0.49229692, 0.49247606,
       0.49264902, 0.49281609, 0.49297753, 0.49313358, 0.49328449,
       0.49343048, 0.49357176, 0.49370853, 0.49384099, 0.4939693 ,
       0.49409364, 0.49421418, 0.49433107, 0.49444444, 0.49455446]))
       '''