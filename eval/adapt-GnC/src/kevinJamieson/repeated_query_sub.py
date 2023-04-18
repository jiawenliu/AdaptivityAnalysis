
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
MEAN = 0.1


def repeated_query_subroutine(delta, strategy, mechanism, epoch = MAX_EPOCH):
    l, queried_set = 0, []
    q = strategy.next_query()
    a = -1
    ans_list, true_ans_list = [], []
    while q and l < epoch:
        delta_l = np.sqrt((l + 1) * np.log(2 / delta) / (2**l))
        for _ in range(2 ** l):
            if q is None:
                break
            r = mechanism.get_answer(q["query"])
            if r[0]["answer"] is not None:
                queried_set.append(r[0]["answer"] * 2.0 - 1)
                pre_ans = [{"answer": np.sum((queried_set)).mean()}]
                q = strategy.next_query(pre_ans)
                true_ans_list = strategy.true_ans_list[-1]
            else:
                q = None
                break
            ans_list.append(strategy.mech_ans_list[-1])
        l = l + 1
        a = abs(np.sum(np.sign(queried_set))) - delta_l
    return true_ans_list, ans_list




def eval_repeated_query_subroutine(delta, n = DATA_SIZE, cardinality = CARDINALITY, q_max = MAX_QUERY_NUM, mechanism = mech.Mechanism()):
    strategy = stg.Strategy(n, q_mean = MEAN, ada_freq = {"method": "repeated_query_subroutine", "method_param": q_max}, q_max = q_max, cardinality = cardinality)
    mechanism.reset()
    mechanism.add_data({'data': strategy.gen_data()})

    true_ans_list, mech_ans_list = repeated_query_subroutine(delta, strategy, mechanism)
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
# Baseline_rmse = [eval_repeated_query_subroutine(repeated_query_sub_delta, n, dimension, q_max, Baseline).mean() for q_max in stepped_q_max]
Baseline_rmse = eval_repeated_query_subroutine(repeated_query_sub_delta, n, dimension, q_max, Baseline)

DataSplit = mech.Mechanism()
DataSplit.add_params(beta=beta, tau=tau)
DataSplit_rmse = eval_repeated_query_subroutine(repeated_query_sub_delta, n, dimension, q_max, DataSplit)

Thresh = mech.Thresholdout_Mechanism(hold_frac=hold_frac, threshold=threshold, sigma=sigma)
Thresh.add_params(beta=beta, tau=tau, check_for_width=None)
# Thresh_rmse = [eval_repeated_query_subroutine(repeated_query_sub_delta, n, dimension, q_max, Thresh).mean() for q_max in stepped_q_max]
Thresh_rmse = eval_repeated_query_subroutine(repeated_query_sub_delta, n, dimension, q_max, Thresh)


Gauss = mech.Gaussian_Mechanism(sigma=sigma)
Gauss.add_params(beta=beta, tau=tau, check_for_width=None)
# Gauss_rmse = [eval_repeated_query_subroutine(repeated_query_sub_delta, n, dimension, q_max, Gauss).mean() for q_max in stepped_q_max]
Gauss_rmse = eval_repeated_query_subroutine(repeated_query_sub_delta, n, dimension, q_max, Gauss)

print(Baseline_rmse, DataSplit_rmse, Gauss_rmse, Thresh_rmse)

# plt.figure()
# x_list = range(10, 101, 10)

# plt.plot(Baseline_rmse, 'g', label= "empirical")
# # plt.plot(x_list, DataSplit_rmse, 'y', label= "DataSplit")
# plt.plot(Thresh_rmse, 'y', label= "Threshold")
# plt.plot(Gauss_rmse, 'r', label= "Gaussian - Adaptfun")
# # plt.plot(x_list, GnC_gauss_rmse, 'm', label= "GnC_gauss")
# # plt.plot(x_list, GnC_thresh_rmse, 'c', label= "GnC_thresh")
# # plt.plot(x_list, GnC_DataSplit_rmse, label= "GnC_DataSplit")
# plt.xlabel("Queries")
# plt.ylabel("RMSE (Generalization Error) for adaptive queries")
# plt.legend()
# plt.grid()
# plt.savefig("../../plots/repeated_query_subroutine-test-2.png")
# plt.show()


# DataSplit = mech.Mechanism()
# DataSplit.add_params(beta=beta, tau=tau)
# DataSplit_rmse = eval_repeated_query_subroutine(repeated_query_sub_delta, n, dimension, q_max, DataSplit)

# print(DataSplit_rmse)

'''
(array([  0. ,   0.5,   0.5, ..., 242. , 242.5, 243. ]), 
array([ 0.5,  0.5,  0.5,  0.5,  0.5,  1. ,  1. ,  1. ,  1.5,  2. ,  2. ,
        2. ,  2.5,  3. ,  3. ,  3. ,  3. ,  3. ,  3. ,  3.5,  4. ,  4.5,
        5. ,  5. ,  5.5,  6. ,  6. ,  6.5,  6.5,  7. ,  7. ,  7. ,  7. ,
        7.5,  8. ,  8. ,  8. ,  8. ,  8. ,  8.5,  8.5,  8.5,  9. ,  9.5,
       10. , 10.5, 11. , 11.5, 11.5, 12. , 12.5, 13. , 13. , 13.5, 13.5,
       13.5, 14. , 14. , 14.5, 14.5, 15. , 15. , 15.5, 16. , 16.5, 16.5,
       17. , 17. , 17. , 17. , 17. , 17. , 17. , 17. , 17. , 17.5, 18. ,
       18. , 18.5, 18.5, 18.5, 19. , 19.5, 20. , 20. , 20. , 20. , 20. ,
       20. , 20. , 20. , 20. , 20. , 20.5, 20.5, 20.5, 21. , 21.5, 21.5,
       21.5]), 
array([ 1.        ,  0.        ,  1.        , ..., 18.11323378,
       19.11323378, 18.55692376]), 
array([  0.        ,   1.        ,   2.        , ..., 186.47238279,
       187.47238279, 187.47238279]))
'''


'''
array([  0.89442719,   0.89442719,   1.39442719, ..., 214.89442719,
       214.89442719, 215.39442719]), 
       array([ 0.89442719,  0.89442719,  1.39442719,  1.39442719,  1.89442719,
        1.89442719,  1.89442719,  1.89442719,  2.39442719,  2.89442719,
        3.39442719,  3.39442719,  3.89442719,  4.39442719,  4.89442719,
        5.39442719,  5.39442719,  5.39442719,  5.39442719,  5.89442719,
        6.39442719,  6.39442719,  6.39442719,  6.39442719,  6.39442719,
        6.39442719,  6.89442719,  7.39442719,  7.89442719,  7.89442719,
        8.39442719,  8.89442719,  8.89442719,  8.89442719,  8.89442719,
        8.89442719,  8.89442719,  9.39442719,  9.89442719,  9.89442719,
       10.39442719, 10.39442719, 10.39442719, 10.89442719, 11.39442719,
       11.89442719, 11.89442719, 11.89442719, 11.89442719, 11.89442719,
       11.89442719, 11.89442719, 12.39442719, 12.39442719, 12.39442719,
       12.89442719, 13.39442719, 13.89442719, 13.89442719, 14.39442719,
       14.39442719, 14.39442719, 14.89442719, 15.39442719, 15.39442719,
       15.89442719, 16.39442719, 16.89442719, 16.89442719, 17.39442719,
       17.89442719, 17.89442719, 17.89442719, 17.89442719, 17.89442719,
       17.89442719, 17.89442719, 17.89442719, 17.89442719, 17.89442719,
       18.39442719, 18.89442719, 19.39442719, 19.39442719, 19.39442719,
       19.89442719, 20.39442719, 20.89442719, 20.89442719, 21.39442719,
       21.89442719, 21.89442719, 22.39442719, 22.39442719, 22.89442719,
       23.39442719, 23.39442719, 23.39442719, 23.89442719, 23.89442719]), 
       array([0.10557281, 1.10557281, 2.10557281, ..., 9.43755123, 8.43755123,
       9.43755123]),
       array([  1.39442719,   1.39442719,   1.39442719, ..., 176.14060316,
       176.14060316, 176.64060316])
       
       '''