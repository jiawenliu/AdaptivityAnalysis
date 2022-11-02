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


class Runner():
	"""
	Base class which runs the analyst strategies. 
	Compatible with the Quandaric-Strategy and the mechanisms having fixed
	dataset size n, desired significance beta (coverage should be at least 1-beta), and tolerance width tau.
	""" 

	def __init__(self):
	    self.q_cnt = 0

	def one_run_one_mech(self, n, mechanism):
		strategy = stg.Strategy(n,  ada_freq = {"method": "additive", "method_param": 10000}, q_max=10000)
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
		print("ANS OF ADAPTIVE QUERYS:", strategy.mech_ans_list)
		print("TRUE ANSWER OF ADAPTIVE QUERYS:", strategy.true_ans_list)
		SE = np.square(np.subtract(strategy.true_ans_list[:q_done], strategy.mech_ans_list)).mean()
		return SE

	def runs_one_mech(self, n_list = range(500, 1001, 100), runs = 1, mechanism = mech.Mechanism()):

	    rmse_list = []
	    for n in n_list:
	    	se_list = [(self.one_run_one_mech(n, mechanism)) for _ in range(runs)]
	    	print("SQUARE ERROR: ", se_list)
	        rmse = math.sqrt(np.array(filter(lambda x: x is not None, se_list)).mean())
	    	print("ROOT MEAN SQUARE ERROR: ", rmse)
	        rmse_list.append(rmse)

	    print("DATA SIZE RANGE: ", n_list)
	    print("RMSE: ", rmse_list)
	    return rmse_list

	def main(self):
		runs = 10
		n_list = range(200, 1001, 100)
		beta, tau = 0.05, 0.1

	        
	##################################################################################################################
	###################################### Switching Mechanisms: ######################################
	##################################################################################################################
		f = open('../results/test.txt', 'rb+')
	# ###################################### Emperical Result: ######################################

		Baseline = mech.Mechanism()
		Baseline.add_params(beta=beta, tau=tau, check_for_width=None)
		Baseline_rmse = self.runs_one_mech(n_list, runs, Baseline)
		print("BASELINE")
		f.write("BASELINE: " + str(Baseline_rmse) + '\n')
	###################################### Switching Mechanisms Above ######################################
		f.close()

		def plot():
			plt.plot(n_list, Baseline_rmse, 'r', label= "Baseline")
			# plt.plot(n_list, DataSplit_rmse, 'y', label= "DataSplit")
			# plt.plot(n_list, Thresh_rmse, 'g', label= "Thresh")
			# plt.plot(n_list, Gauss_rmse, 'b', label= "Gauss")
			# plt.plot(n_list, GnC_gauss_rmse, 'm', label= "GnC_gauss")
			# plt.plot(n_list, GnC_thresh_rmse, 'c', label= "GnC_thresh")
			plt.xlabel("No. of Samples")
			plt.ylabel("Generalization Error (RMSE)")
			plt.legend()
			plt.grid()
			plt.ylim(-0.1, 0.8)

			plt.savefig("../plots/test.png")
			plt.show()

		plot()


r = Runner()
r.main()
