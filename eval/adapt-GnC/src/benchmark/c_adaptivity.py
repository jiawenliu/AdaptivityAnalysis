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


DATA_SIZE = 1000
CARDINALITY = 1000
MAX_QUERY_NUM = 1000
MAX_EPOCH = 100
MEAN = 0.5


def c_adaptivity(strategy, mechanism, epoch = MAX_EPOCH):
    l, queried_set = 0, []
    q = strategy.next_query()
    while q and l < epoch:
        if q is None:
            break
        r = mechanism.get_answer(q["query"])
        if r[0]["answer"] is not None:
            queried_set.append((r[0]["answer"] * 2.0 - 1))
            pre_ans = [{"answer": np.sign(np.sum((queried_set)))}]
            q = strategy.next_query(pre_ans)
        else:
            q = None
            break
        l = l + 1
    return 




def eval_c_adaptivity(n = DATA_SIZE, cardinality = CARDINALITY, q_max = MAX_QUERY_NUM, mechanism = mech.Mechanism()):
    strategy = stg.Strategy(n, q_mean = MEAN, ada_freq = {"method": "c_adaptivity", "method_param": q_max}, q_max = q_max, cardinality = cardinality)
    mechanism.reset()
    mechanism.add_data({'data': strategy.gen_data()})

    c_adaptivity(strategy, mechanism)
    q_done = len(strategy.mech_ans_list)
    se = np.square(np.subtract(strategy.true_ans_list[:q_done], strategy.mech_ans_list))
    mse = [se[:i].mean() for i in range(q_done)]

    return np.sqrt(mse)

n = 100
dimension = 100
q_max = 100
runs = 10

stepped_q_max = range(q_max/2, q_max, 10)

beta, tau = 0.05, 1.0
gaussian_sigma, laplace_sigma = 0.7, 0.7
hold_frac, threshold, check_data_frac = 0.5, 0.5, 0.05

repeated_query_sub_delta = 0.1

Baseline = mech.Mechanism()
Baseline.add_params(beta=beta, tau=tau, check_for_width=None)
Baseline_rmse = np.mean([eval_c_adaptivity(n, dimension, q_max, Baseline) for _ in range(runs)], axis = 0)

DataSplit = mech.Mechanism(q_max)
DataSplit.add_params(beta=beta, tau=tau)
DataSplit_rmse = eval_c_adaptivity(n*10, dimension, q_max, DataSplit)

Thresh = mech.Thresholdout_Mechanism(hold_frac=hold_frac, threshold=threshold, sigma=laplace_sigma)
Thresh.add_params(beta=beta, tau=tau, check_for_width=None)
# Thresh_rmse = [eval_c_adaptivity(n, dimension, q_max, Thresh).mean() for q_max in stepped_q_max]
# Thresh_rmse = eval_c_adaptivity(n, dimension, q_max, Thresh)
Thresh_rmse = np.mean([eval_c_adaptivity(n, dimension, q_max, Thresh) for _ in range(runs)], axis = 0)


Gauss = mech.Gaussian_Mechanism(sigma=gaussian_sigma)
Gauss.add_params(beta=beta, tau=tau, check_for_width=None)
# Gauss_rmse = [eval_c_adaptivity(n, dimension, q_max, Gauss).mean() for q_max in stepped_q_max]
# Gauss_rmse = eval_c_adaptivity(n, dimension, q_max, Gauss)
Gauss_rmse = np.mean([eval_c_adaptivity(n, dimension, q_max, Gauss) for _ in range(runs)], axis = 0)



print(Baseline_rmse, DataSplit_rmse, Gauss_rmse, Thresh_rmse)
print(Baseline_rmse[1:].mean(), DataSplit_rmse[1:].mean(), Gauss_rmse[1:].mean(), Thresh_rmse[1:].mean())


plt.figure()
x_list = range(10, 101, 10)

plt.plot(Baseline_rmse, 'g', label= "empirical")
# plt.plot(x_list, DataSplit_rmse, 'y', label= "DataSplit")
plt.plot(Thresh_rmse, 'y', label= "Threshold - Adaptfun")
plt.plot(Gauss_rmse, 'r', label= "Gaussian ")
# plt.plot(x_list, GnC_gauss_rmse, 'm', label= "GnC_gauss")
# plt.plot(x_list, GnC_thresh_rmse, 'c', label= "GnC_thresh")
# plt.plot(x_list, GnC_DataSplit_rmse, label= "GnC_DataSplit")
plt.xlabel("Queries")
plt.ylabel("RMSE (Generalization Error) for adaptive queries")
plt.legend()
plt.grid()
plt.savefig("../../plots/c_adaptivity-test.png")
plt.show()

'''
(1.9086507780159632, 1.7614247555561848, 1.5663894855613751, 1.7956262696990717)
'''

'''
(1.3925445142956712, 1.4683140617990915, 1.4148367353420204, 1.402582457459642)
'''