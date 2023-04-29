# -*- coding: utf-8 -*-

from collections import OrderedDict
import cw_funcs as cw
import helper_funcs as hf
import strategies as stg
import mechanisms as mech
import numpy as np
import os
import scipy as sc
import math
import matplotlib.pyplot as plt
from signal import signal, SIGPIPE, SIG_DFL
signal(SIGPIPE, SIG_DFL)


class LRGD():
	"""
	Base class which runs the analyst strategies. 
	Compatible with the Quandaric-Strategy and the mechanisms having fixed
	dataset size n, desired significance beta (coverage should be at least 1-beta), and tolerance width tau.
	""" 

	def __init__(self):
		self.q_cnt = 0

	def one_run_one_mech(self, n, q_max = 1000, q_adapt = 1000, mechanism = mech.Mechanism()):
		strategy = stg.Strategy(n,  ada_freq = {"method": "additive", "method_param": q_adapt}, q_max=q_max)
		mechanism.reset()
		mechanism.add_data({'data': strategy.gen_data()})

		q = strategy.next_query()
		while q:
			r = mechanism.get_answer(q["query"])
			if r[0]["answer"] is not None:
				q = strategy.next_query(r)
			else:
				q = None
		q_done = len(strategy.mech_ans_list)
		# print("ANS OF ADAPTIVE QUERYS:", strategy.mech_ans_list)
		# print("TRUE ANSWER OF ADAPTIVE QUERYS:", strategy.true_ans_list)
		square_errors = np.square(np.subtract(strategy.true_ans_list[:q_done], strategy.mech_ans_list))
		print("Complete one run, with Squre Errors: {}".format(square_errors))
		return list(square_errors)

	def runs_one_mech(self, n_list = range(500, 1001, 100), q_max_list = range(500, 1001, 100), q_adapt_list = range(500, 1001, 100), runs = 1, mechanism = mech.Mechanism()):

		rmse_list = []
		for n in n_list:
			for i in range(len(q_max_list)):
				se_matrix = [(self.one_run_one_mech(n, q_max_list[i], q_adapt_list[i], mechanism)) for _ in range(runs)]
				rmse_pair_list = [np.array([[l[i],1] if len(l) > i else [0, 0] for l in se_matrix]).sum(0) for i in range(max([len(l) for l in se_matrix]))]
				rmse = [pl[0] / pl[1] for pl in rmse_pair_list]
				print("ROOT MEAN SQUARE ERROR: ", rmse)
				rmse_list.append(rmse)

		print("DATA SIZE RANGE: ", n_list)
		print("RMSE: ", rmse_list)
		return rmse_list



q_max_list = [5000]
q_adapt_list = [200]

runs = 5
n_list = [1000]
beta, tau = 0.05, 1.0
sigma = 0.03
hold_frac, threshold, check_data_frac = 0.5, 0.05, 0.05


r = LRGD()
DataSplit = mech.Mechanism()
DataSplit.add_params(beta=beta, tau=tau)
mech_rmse = r.runs_one_mech(np.array(n_list)*3, q_max_list, q_adapt_list, runs, DataSplit)
r.runs_one_mech(q_max_list = [1000], q_adapt_list = [2], mechanism = DataSplit)


		
