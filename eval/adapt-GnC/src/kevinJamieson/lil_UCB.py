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
	def __init__(self, epsilon = EPS, lam = EPS, beta = BETA, sigma = SIGMA, confidential_interval = CONFIDENTIAL_INTERVAL):
		self.epsilon = epsilon
		self.lam = lam
		self.beta = beta
		self.sigma = sigma
		self.confidential_interval = confidential_interval

def lil_ucb (strategy, mechanism, para = Para()):
	data_size = strategy.n
	time_gate = [1]*data_size
	def gate_thresh(curr_time_gate):
		for i in range(data_size):
			gate = curr_time_gate[i]
			if gate > 1 + para.lam * (sum(curr_time_gate) - gate):
				return False
		return True
	pre_ans = [{"para" : para}]
	while gate_thresh(time_gate):
		query_result = []
		for i in range(data_size):
			pre_ans[0]["gate"] = time_gate[i]
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
				time_gate[i] = time_gate[i] + 1
	return np.argmax(time_gate)





def eval_lil_ucb(n = DATA_SIZE, cardinality = CARDINALITY, q_max = MAX_QUERY_NUM, mechanism = mech.Mechanism()):
    strategy = stg.Strategy(n, q_mean = MEAN, ada_freq = {"method": "lil_ucb", "method_param": q_max}, q_max = q_max, cardinality = cardinality)
    mechanism.add_data({'data': strategy.gen_data_decimal()})
    para = Para(0.5, 0.5, 0.5, 0.5, 0.2)
    lil_ucb(strategy, mechanism, para)
    q_done = min(len(strategy.true_ans_list), len(strategy.mech_ans_list))
    mse = np.square(np.subtract(strategy.true_ans_list[:q_done], strategy.mech_ans_list[:q_done]))
    
    rmse = 	np.sqrt(mse)
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
    return rmse
    



'''
Parameters of Strategies
'''
n = 10
dimension = 1
q_max = 10


'''
Parameters of Mechanisms
'''
beta, tau = 0.05, 1.0
sigma = 1.0
hold_frac, threshold, check_data_frac = 0.7, 0.05, 0.05


Baseline = mech.Mechanism()
Baseline.add_params(beta=beta, tau=tau, check_for_width=None)
# Baseline_rmse = [eval_lil_ucb(n, dimension, q_max, Baseline).mean() for q_max in stepped_q_max]
Baseline_rmse = eval_lil_ucb(n, dimension, q_max, Baseline)

DataSplit = mech.Mechanism(max_q = q_max)
DataSplit.add_params(beta=beta, tau=tau)
DataSplit_rmse = eval_lil_ucb(n, dimension, q_max, DataSplit)

Thresh = mech.Thresholdout_Mechanism(hold_frac=hold_frac, threshold=threshold, sigma=sigma)
Thresh.add_params(beta=beta, tau=tau, check_for_width=None)
# Thresh_rmse = [eval_lil_ucb(n, dimension, q_max, Thresh).mean() for q_max in stepped_q_max]
Thresh_rmse = eval_lil_ucb(n, dimension, q_max, Thresh)


Gauss = mech.Gaussian_Mechanism(sigma=sigma)
Gauss.add_params(beta=beta, tau=tau, check_for_width=None)
# Gauss_rmse = [eval_lil_ucb(n, dimension, q_max, Gauss).mean() for q_max in stepped_q_max]
Gauss_rmse = eval_lil_ucb(n, dimension, q_max, Gauss)

print(Baseline_rmse, DataSplit_rmse, Gauss_rmse, Thresh_rmse)
print(Baseline_rmse.mean(), DataSplit_rmse.mean(), Gauss_rmse.mean(), Thresh_rmse.mean())

'''
(2.2879070854805676, 1.8580622429961056, 0.47574304704756065, 1.4555054821829276)
'''

'''
(2.1425024651952835, 2.2669679225188535, 0.6714195040422586, 1.353000656266134)
'''

'''
3.0174285334885935 3.1370326709861 3.5245510505341158 2.386545799138394
'''