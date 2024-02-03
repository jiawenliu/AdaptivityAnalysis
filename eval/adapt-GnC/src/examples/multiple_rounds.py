
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

# strategy = stg.Strategy(n,  ada_freq = {"method": "additive", "method_param": q_adapt}, para=para)
DATA_SIZE = 1000
CARDINALITY = 1000
MAX_QUERY_NUM = 1000
MAX_EPOCH = 100
MEAN = 0.5
class Para:
	def __init__(self, max_iteration = MAX_EPOCH, population = CARDINALITY, control_size = CARDINALITY):
		self.traced = set()
		self.max_iteration = max_iteration
		self.population = population
		self.control_size = control_size
		self.n_score = [0.0] * self.population
		self.c_score = [0.0] * self.control_size

def multiple_rounds (strategy, mechanism, para = Para()):
	pre_ans = [{"para" : para}]
	k = 0
	while k < para.max_iteration:
		k = k + 1
		q = strategy.next_query(pre_ans)
		if q is None:
			break
		r = mechanism.get_answer(q["query"])
		if r[0]["answer"] is not None:
			a, pre_ans[0]["answer"] = r[0]["answer"], r[0]["answer"]
			para.n_score = [score if i in para.traced else score + (a - strategy.q_mean)* (np.random.choice([0, 1.0], p =  [1 - strategy.pr_1, strategy.pr_1]) - strategy.q_mean) for i,score in enumerate(para.n_score)]
			para.c_score = [score + (a - strategy.q_mean)* (np.random.choice([0, 1.0], p =  [1 - strategy.pr_1, strategy.pr_1]) - strategy.q_mean) for score in para.c_score]
			for i,score in enumerate(para.n_score):
				if score > max(para.c_score):
					para.traced.add(i)
		else:
			q = None
			break

	return r[0]["answer"]




def eval_multiple_rounds(n = DATA_SIZE, cardinality = CARDINALITY, para = Para(), mechanism = mech.Mechanism()):
    para = Para(max_iteration = 10, population = 200, control_size = 10)
    strategy = stg.Strategy(n, q_mean = MEAN, ada_freq = {"method": "multiple_rounds", "method_param": para}, q_max = MAX_QUERY_NUM, cardinality = para.population)
    mechanism.reset()
    mechanism.max_q = para.max_iteration
    mechanism.add_data({'data': strategy.gen_data_integer()})

    multiple_rounds(strategy, mechanism, para)
    print(len(para.traced))
    q_done = min(len(strategy.true_ans_list), len(strategy.mech_ans_list))
    mse = np.square(np.subtract(strategy.true_ans_list[:q_done], strategy.mech_ans_list[:q_done]))
    rmse = np.sqrt(mse)[-1]
    true_data = strategy.true_ans_list[:q_done]
    std = np.std(true_data)
    amax = np.amax(true_data)
    amin = np.amin(true_data)
    dif= amax - amin  
    mean = np.mean(true_data)
    print("dif", dif)
    print("mean", mean)
    print("rmse",rmse)
    nrmse = rmse/std
    nrmse1 = rmse/dif
    nrmse2 = rmse/mean
    print("std", std)
    print("nrmse",nrmse)
    print("nrmse1", nrmse1)
    print("nrmse2", nrmse2)
    return nrmse2


n = 1000
cardinality = n
runs = 10

beta, tau = 0.05, 1.0
sigma = 0.015
hold_frac, threshold, check_data_frac = 0.7, 0.05, 0.05


Baseline = mech.Mechanism()
Baseline.add_params(beta=beta, tau=tau, check_for_width=None)
Baseline_rmse = np.mean([eval_multiple_rounds(n = n, cardinality = cardinality, mechanism = Baseline) for _ in range(runs)])
# Baseline_rmse = eval_multiple_rounds(n = n, cardinality = cardinality, para = para, mechanism = Baseline)
print(Baseline_rmse)

DataSplit = mech.Mechanism()
DataSplit.add_params(beta=beta, tau=tau)
DataSplit_rmse = np.mean([eval_multiple_rounds(n = n*n, cardinality = cardinality, mechanism = DataSplit) for _ in range(runs)])
# DataSplit_rmse = eval_multiple_rounds(n = n , cardinality = cardinality, para = para, mechanism = DataSplit)

Thresh = mech.Thresholdout_Mechanism(hold_frac=hold_frac, threshold=threshold, sigma=sigma)
Thresh.add_params(beta=beta, tau=tau, check_for_width=None)

Thresh_rmse = np.mean([eval_multiple_rounds(n = n, cardinality = cardinality, mechanism = Thresh) for _ in range(runs)])

# Thresh_rmse = eval_multiple_rounds(n = n, cardinality = cardinality, para = para, mechanism = Thresh)


Gauss = mech.Gaussian_Mechanism(sigma=sigma)
Gauss.add_params(beta=beta, tau=tau, check_for_width=None)
Gauss_rmse = np.mean([eval_multiple_rounds(n = n, cardinality = cardinality, mechanism = Gauss) for _ in range(runs)])
# Gauss_rmse = eval_multiple_rounds(n = n, cardinality = cardinality, para = para, mechanism = Gauss)
print(Gauss_rmse)

print(Baseline_rmse, DataSplit_rmse, Gauss_rmse, Thresh_rmse)

'''
(0.16, 0.15454545454545454, 0.10867477551523193, 0.10350775295216555)
'''