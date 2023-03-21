
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

def repeated_query_subroutine(delta, strategy, mechanism):
    l, queried_set = 0, []
    q = strategy.next_query()
    print(q)
    a = -1
    while q and a < 0:
        delta_l = np.sqrt((l + 1) * np.log(2/delta) / (2**l))
        for _ in range(2**l):
            r = mechanism.get_answer(q["query"])
            if r[0]["answer"] is not None:
                queried_set.append(r[0]["answer"] * 2.0 - 1)
                pre_ans = [{"answer": np.sum((queried_set)).mean()}]
                q = strategy.next_query(pre_ans)
            else:
                q = None
                break
        l = l + 1
        a = abs(np.sum(np.sign(queried_set))) - delta_l
    return 

DATA_SIZE = 1000
CARDINALITY = 1000
MAX_QUERY_NUM = 1000



def eval_repeated_query_subroutine(delta, n = DATA_SIZE, cardinality = CARDINALITY, q_max = MAX_QUERY_NUM, mechanism = mech.Mechanism()):
    strategy = stg.Strategy(n, q_mean = 0.5, ada_freq = {"method": "repeated_query_subroutine", "method_param": q_max}, q_max = q_max, cardinality = cardinality)
    mechanism.reset()
    mechanism.add_data({'data': strategy.gen_data()})

    repeated_query_subroutine(delta, strategy, mechanism)
    mse = np.square(np.subtract(strategy.true_ans_list[:-1],strategy.mech_ans_list))
    print(strategy.true_ans_list, strategy.mech_ans_list)
    # print(mse)
    # print(np.sqrt(mse))

    return np.sqrt(mse)

n = 100
dimension = 100
q_max = 10000
runs = 10

stepped_q_max = range(q_max/2, q_max, 10)

beta, tau = 0.05, 1.0
sigma = 0.03
hold_frac, threshold, check_data_frac = 0.5, 0.05, 0.05

repeated_query_sub_delta = 0.1

Baseline = mech.Mechanism()
Baseline.add_params(beta=beta, tau=tau, check_for_width=None)
# Baseline_rmse = [eval_repeated_query_subroutine(repeated_query_sub_delta, n, dimension, q_max, Baseline).mean() for q_max in stepped_q_max]
Baseline_rmse = eval_repeated_query_subroutine(repeated_query_sub_delta, n, dimension, q_max, Baseline)

# DataSplit = mech.Mechanism()
# DataSplit.add_params(beta=beta, tau=tau)
# DataSplit_rmse = eval_repeated_query_subroutine(repeated_query_sub_delta, n*10, dimension, q_max, DataSplit)

Thresh = mech.Thresholdout_Mechanism(hold_frac=hold_frac, threshold=threshold, sigma=sigma)
Thresh.add_params(beta=beta, tau=tau, check_for_width=None)
# Thresh_rmse = [eval_repeated_query_subroutine(repeated_query_sub_delta, n, dimension, q_max, Thresh).mean() for q_max in stepped_q_max]
Thresh_rmse = eval_repeated_query_subroutine(repeated_query_sub_delta, n, dimension, q_max, Thresh)


Gauss = mech.Gaussian_Mechanism(sigma=sigma)
Gauss.add_params(beta=beta, tau=tau, check_for_width=None)
# Gauss_rmse = [eval_repeated_query_subroutine(repeated_query_sub_delta, n, dimension, q_max, Gauss).mean() for q_max in stepped_q_max]
Gauss_rmse = eval_repeated_query_subroutine(repeated_query_sub_delta, n, dimension, q_max, Gauss)


plt.figure()
x_list = range(10, 101, 10)

plt.plot(Baseline_rmse, 'g', label= "empirical")
# plt.plot(x_list, DataSplit_rmse, 'y', label= "DataSplit")
plt.plot(Thresh_rmse, 'y', label= "Threshold")
plt.plot(Gauss_rmse, 'r', label= "Gaussian - Adaptfun")
# plt.plot(x_list, GnC_gauss_rmse, 'm', label= "GnC_gauss")
# plt.plot(x_list, GnC_thresh_rmse, 'c', label= "GnC_thresh")
# plt.plot(x_list, GnC_DataSplit_rmse, label= "GnC_DataSplit")
plt.xlabel("Queries")
plt.ylabel("RMSE (Generalization Error) for adaptive queries")
plt.legend()
plt.grid()
plt.savefig("../../plots/repeated_query_subroutine-test2.png")
plt.show()
