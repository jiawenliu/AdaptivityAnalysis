
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
    while q and l < epoch:
        delta_l = np.sqrt((l + 1) * np.log(2 / delta) / (2**l))
        if q is None:
            break
        print(q)
        r = mechanism.get_answer(q["query"])
        print(r)
        if r[0]["answer"] is not None:
            queried_set.append(r[0]["answer"] * 2.0 - 1)
            # if "empirical_answer" in r[0].keys():
            #     pre_ans = [{"answer": 1.0 if (np.sign(np.mean(queried_set)) == np.sign(r[0]["empirical_answer"])) else 0.0}]
            
            # else:
            # pre_ans = [{"answer": 1.0 if (np.sign(np.mean(queried_set)) == np.sign(r[0]["answer"])) else 0.0}]
            pre_ans = [{"answer": np.sum((queried_set)).mean()}]
            q = strategy.next_query(pre_ans)
        else:
            q = None
            break
        l = l + 1
        a = abs(np.sum(np.sign(queried_set))) - delta_l
    return 




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
