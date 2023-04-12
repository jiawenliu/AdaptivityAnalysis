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
CARDINALITY = 1000
MAX_QUERY_NUM = 1000
MAX_EPOCH = 100


def lil_ucb (epsilon, lam, beta, sigma, confidential_interval,  n, data):
	time_gate = [1]*n
	def gate_thresh(curr_time_gate):
		for i in range(n):
			gate = curr_time_gate[i]
			if gate > 1 + lam * (sum(curr_time_gate) - gate):
				return False
		return True
	
	while gate_thresh(time_gate):
		query_result = []
		for i in range(n):
			gate = time_gate[i]
			
			def query(data):
				return 1.0 / gate * sum(data[:gate]) + (1 + beta) * (1 + math.sqrt(epsilon)) * math.sqrt(2 * (sigma**2) * (1 + epsilon) * math.log(math.log((1 + epsilon) * gate)/confidential_interval) / gate)
			
			query_result.append(query(data)) 

		arm = np.argmax(query_result)
		for i in range(n):

			if arm == i:
				time_gate[i] = time_gate[i] + 1
	return np.argmax(time_gate)





def eval_lil_ucb(delta, n = DATA_SIZE, cardinality = CARDINALITY, q_max = MAX_QUERY_NUM, mechanism = mech.Mechanism()):
    strategy = stg.Strategy(n, q_mean = 0.5, ada_freq = {"method": "lil_ucb", "method_param": q_max}, q_max = q_max, cardinality = cardinality)
    mechanism.reset()
    mechanism.add_data({'data': strategy.gen_data()})

    true_ans_list, mech_ans_list = lil_ucb(delta, strategy, mechanism)
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
# Baseline_rmse = [eval_lil_ucb(repeated_query_sub_delta, n, dimension, q_max, Baseline).mean() for q_max in stepped_q_max]
Baseline_rmse = eval_lil_ucb(repeated_query_sub_delta, n, dimension, q_max, Baseline)

DataSplit = mech.Mechanism()
DataSplit.add_params(beta=beta, tau=tau)
DataSplit_rmse = eval_lil_ucb(repeated_query_sub_delta, n, dimension, q_max, DataSplit)

Thresh = mech.Thresholdout_Mechanism(hold_frac=hold_frac, threshold=threshold, sigma=sigma)
Thresh.add_params(beta=beta, tau=tau, check_for_width=None)
# Thresh_rmse = [eval_lil_ucb(repeated_query_sub_delta, n, dimension, q_max, Thresh).mean() for q_max in stepped_q_max]
Thresh_rmse = eval_lil_ucb(repeated_query_sub_delta, n, dimension, q_max, Thresh)


Gauss = mech.Gaussian_Mechanism(sigma=sigma)
Gauss.add_params(beta=beta, tau=tau, check_for_width=None)
# Gauss_rmse = [eval_lil_ucb(repeated_query_sub_delta, n, dimension, q_max, Gauss).mean() for q_max in stepped_q_max]
Gauss_rmse = eval_lil_ucb(repeated_query_sub_delta, n, dimension, q_max, Gauss)

print(Baseline_rmse, DataSplit_rmse, Gauss_rmse, Thresh_rmse)
