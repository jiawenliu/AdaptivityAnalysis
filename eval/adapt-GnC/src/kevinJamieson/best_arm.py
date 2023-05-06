# -*- coding: utf-8 -*-


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

from signal import signal, SIGPIPE, SIG_DFL
signal(SIGPIPE, SIG_DFL)

import sys
sys.path.append("..")

DATA_SIZE = 1000
CARDINALITY = 1
MAX_QUERY_NUM = 1000
MAX_EPOCH = 100

EPS = 0.1
EPS = 0.1 
BETA = 0.1
CONFIDENTIAL_INTERVAL = 0.9
SIGMA = 0.1
MEAN = 0.1


'''
Default Value:
EPS = 0.1
LAM = 0.1 
BETA = 0.1
CONFIDENTIAL_INTERVAL = 0.9
SIGMA = 0.1
'''
class Para:
	def __init__(self, arm_nums = CARDINALITY, epsilon = EPS, lam = EPS, beta = BETA, sigma = SIGMA, confidential_interval = CONFIDENTIAL_INTERVAL):
		self.epsilon = epsilon
		self.lam = lam
		self.beta = beta
		self.sigma = sigma
		self.confidential_interval = confidential_interval
		self.arm_nums = arm_nums
		self.gate = [1] * self.arm_nums
		self.curr_arm = 0
		self.arms = np.random.rand(self.arm_nums)

	def reset_para(self):
		self.gate = [1] * self.arm_nums
		self.curr_arm = 0


def best_arm (strategy, mechanism, para = Para()):
	data_size = strategy.n
	def gate_thresh(gate):
		for g in gate:
			if g > 1 + para.lam * (sum(gate) - g):
				return False
		return True
	pre_ans = [{"para" : para}]
	while gate_thresh(para.gate):
		query_result = []
		for i in range(para.arm_nums):
			para.curr_arm = i
			q = strategy.next_query(pre_ans)
			if q is None:
				break
			r = mechanism.get_answer(q["query"])
			if r[0]["answer"] is not None:
				query_result.append(r[0]["answer"])
				pre_ans = [{"answer": r[0]["answer"], "para" : para}]
				# true_ans_list = strategy.true_ans_list[-1]
			else:
				q = None
				break
		if q is None:
			break

		arm = np.argmax(query_result)
		for i in range(n):
			if arm == i:
				para.gate[i] = para.gate[i] + 1
	return np.argmax(para.gate)





def eval_best_arm(n = DATA_SIZE, cardinality = CARDINALITY, q_max = MAX_QUERY_NUM, mechanism = mech.Mechanism()):
    strategy = stg.Strategy(n, q_mean = MEAN, ada_freq = {"method": "best_arm", "method_param": q_max}, q_max = q_max, cardinality = cardinality)
    para = Para(cardinality, 0.5, 0.5, 0.5, 0.5, 0.2)
    mechanism.add_data({'data': strategy.gen_data_arm_samples(para.arms)})
    best_arm(strategy, mechanism, para)
    q_done = min(len(strategy.true_ans_list), len(strategy.mech_ans_list))
    mse = np.square(np.subtract(strategy.true_ans_list[:q_done], strategy.mech_ans_list[:q_done]))

    return np.sqrt(mse)

q_max = 1000
n = q_max
dimension = 100


beta, tau = 0.05, 1.0
sigma = 3.5
hold_frac, threshold, check_data_frac = 0.7, 0.05, 0.05


Baseline = mech.Mechanism()
Baseline.add_params(beta=beta, tau=tau, check_for_width=None)
# Baseline_rmse = [eval_best_arm(n, dimension, q_max, Baseline).mean() for q_max in stepped_q_max]
Baseline_rmse = eval_best_arm(n, dimension, q_max, Baseline)

DataSplit = mech.Mechanism(max_q = q_max)
DataSplit.add_params(beta=beta, tau=tau)
DataSplit_rmse = eval_best_arm(n, dimension, q_max, DataSplit)

Thresh = mech.Thresholdout_Mechanism(hold_frac=hold_frac, threshold=threshold, sigma=sigma)
Thresh.add_params(beta=beta, tau=tau, check_for_width=None)
# Thresh_rmse = [eval_best_arm(n, dimension, q_max, Thresh).mean() for q_max in stepped_q_max]
Thresh_rmse = eval_best_arm(n, dimension, q_max, Thresh)


Gauss = mech.Gaussian_Mechanism(sigma=sigma)
Gauss.add_params(beta=beta, tau=tau, check_for_width=None)
# Gauss_rmse = [eval_best_arm(n, dimension, q_max, Gauss).mean() for q_max in stepped_q_max]
Gauss_rmse = eval_best_arm(n, dimension, q_max, Gauss)

print(Baseline_rmse, DataSplit_rmse, Gauss_rmse, Thresh_rmse)
print(Baseline_rmse.mean(), DataSplit_rmse.mean(), Gauss_rmse.mean(), Thresh_rmse.mean())


'''
1.8235091754167532, 1.3301193714302109, 0.4525140548253183, 1.3357693564662898'''