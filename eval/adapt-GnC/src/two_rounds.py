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


class twoRounds():
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

	def runandplot_with_one_mech(self, q_max_list = [200], q_adapt_list = [20], mech_name = "Baseline"):
		runs = 5
		n_list = [1000]
		beta, tau = 0.05, 1.0
		sigma = 0.03
		hold_frac, threshold, check_data_frac = 0.5, 0.05, 0.05


		def plot(x_list, rmse_list):
			plt.plot(x_list, rmse_list, 'r', label= mech_name)
			plt.xlabel("Queries")
			plt.ylabel("RMSE (Generalization Error) for adaptive queries")
			plt.legend()
			plt.grid()
			# plt.savefig("../plots/" + mech_name + ".png")
			# plt.show()

	##################################################################################################################
	################################################### Mechanisms: ##################################################
	##################################################################################################################
		'''
		Choose the Mechanism, Avablie mechanisms are:
		Baseline : The Empirical result on Original Data without any other operations
		DataSplit : Naive Data Splitting mechanism
		Thresh : Thresholdout mechanism from [DFHPRR'15 (NeurIPS)]
		Gauss : Adds 0-mean Gaussian noise with stddev sigma to each answer query(data), and returns answer truncated in [0.0, 1.0]
		GnC_thresh : 'Guess and Check' (GnC) query-answering mechanism instantiated by Thresholdout mechanism from [DFHPRR'15 (NeurIPS)] (3rd mechanism above)
		GnC_gauss : 'Guess and Check' (GnC) query-answering mechanism instantiated by Standard Gaussian Mechanism (4th mechanism above)
		GnC_DataSplit : 'Guess and Check' (GnC) query-answering mechanism instantiated by the Naive Data Splitting Mechanism (2nd mechanism above)
		GnC_Baseline : 'Guess and Check' (GnC) query-answering mechanism instantiated by the Empirical Query Answer Mechanism (1st mechanism above)
		'''
		mech_name, mech_para, mech_rmse = mech_name, mech_name, [[0.0] * (q_max_list[i] / q_adapt_list[i]) for i in range(len(q_max_list))]
		f = open('../results/'+ mech_name + "test.txt", 'w')
	# ###################################### Emperical Result: ######################################
		if mech_name == "Baseline":
			Baseline = mech.Mechanism()
			Baseline.add_params(beta=beta, tau=tau, check_for_width=None)
			Baseline_rmse = self.runs_one_mech(n_list, q_max_list, q_adapt_list, runs, Baseline)
			mech_para = "Baseline: DATA SIZE: {}, RUNS: {}".format((n_list), runs)
			mech_rmse = self.runs_one_mech(n_list, q_max_list, q_adapt_list, runs, Baseline)
			print(mech_para, mech_rmse)
			f.write(mech_para + ": " + str(mech_rmse) + '\n')
			for i in range(len(q_max_list)):
				querys = range(q_adapt_list[i], q_max_list[i]+1, q_adapt_list[i])
				l = min(len(mech_rmse[i]), len(querys))
				plot(querys[:l], mech_rmse[i][:l])
			return mech_rmse

	# # ###################################### Data Splitting Mechanism: ######################################
		elif mech_name == "DataSplit":

			DataSplit = mech.Mechanism()
			DataSplit.add_params(beta=beta, tau=tau)
			mech_rmse = self.runs_one_mech(np.array(n_list)*3, q_max_list, q_adapt_list, runs, DataSplit)
			mech_para = "DATA SPLIT : BETA: {}, TAU: {}, DATA SIZE: {}, RUNS: {}".format(beta, tau, (n_list), runs)
			print(mech_para, mech_rmse)
			f.write(mech_para + ": " + str(mech_rmse) + '\n')
			for i in range(len(q_max_list)):
				querys = range(q_adapt_list[i], q_max_list[i]+1, q_adapt_list[i])
				l = min(len(mech_rmse[i]), len(querys))
				plot(querys[:l], mech_rmse[i][:l])
			return mech_rmse

	# # ###################################### Thresholdout Mechanism: ######################################

		elif mech_name == "Thresh":
			Thresh = mech.Thresholdout_Mechanism(hold_frac=hold_frac, threshold=threshold, sigma=sigma)
			Thresh.add_params(beta=beta, tau=tau, check_for_width=None)
			mech_rmse = self.runs_one_mech(n_list, q_max_list, q_adapt_list, runs, Thresh)
			mech_para = "THRESH : HOLD_FRAC: {}, T: {}, SIGMA: {}, BETA: {}, TAU: {}, DATA SIZE: {}, RUNS: {}".format(hold_frac, threshold, sigma, beta, tau, n_list, runs)
			print(mech_para, mech_rmse)
			f.write(mech_para + ": " + str(mech_rmse) + '\n')
			for i in range(len(q_max_list)):
				querys = range(q_adapt_list[i], q_max_list[i]+1, q_adapt_list[i])
				l = min(len(mech_rmse[i]), len(querys))
				plot(querys[:l], mech_rmse[i][:l])
			return mech_rmse

	# # ###################################### Gaussian Mechanism: ######################################

		elif mech_name == "Gauss":
			Gauss = mech.Gaussian_Mechanism(sigma=sigma)
			Gauss.add_params(beta=beta, tau=tau, check_for_width=None)
			mech_rmse = self.runs_one_mech(n_list, q_max_list, q_adapt_list, runs, Gauss)
			mech_para = "GAUSS : SIGMA: {}, BETA: {}, TAU: {}, DATA SIZE: {}, RUNS: {}".format(sigma, beta, tau, n_list, runs)
			print(mech_para, mech_rmse)
			f.write(mech_para + ": " + str(mech_rmse) + '\n')
			for i in range(len(q_max_list)):
				querys = range(q_adapt_list[i], q_max_list[i]+1, q_adapt_list[i])
				l = min(len(mech_rmse[i]), len(querys))
				plot(querys[:l], mech_rmse[i][:l])
			return mech_rmse

	# # ###################################### Guess and Check Mechanism instantiated by Gaussian Mechanism: ######################################

		elif mech_name == "GnC_gauss":
		# # TODO: Tuning the parameter
			# Gauss = mech.Gaussian_Mechanism(sigma=sigma)
			# Gauss.add_params(beta=beta, tau=tau)
			GnC_gauss = mech.Guess_and_Check_Mechanism(mech_guess = mech.Gaussian_Mechanism(sigma=sigma),
				check_data_frac = check_data_frac,
				use_mgf_width = False)
			GnC_gauss.add_params(beta = beta, tau = tau)
			mech_rmse = self.runs_one_mech(np.array(n_list), q_max_list, q_adapt_list, runs, GnC_gauss)
			mech_para = "GnC_gauss : SIGMA: {}, CHECK_FRAC: {}, BETA: {}, TAU: {}, DATA SIZE: {}, RUNS: {}".format(sigma, check_data_frac, beta, tau, n_list, runs)
			print(mech_para, mech_rmse)
			f.write(mech_para + ": " + str(mech_rmse) + '\n')
			for i in range(len(q_max_list)):
				querys = range(q_adapt_list[i], q_max_list[i]+1, q_adapt_list[i])
				l = min(len(mech_rmse[i]), len(querys))
				plot(querys[:l], mech_rmse[i][:l])
			return mech_rmse

	# ###################################### Guess and Check Mechanism instantiated by Thresholdout Mechanism: ######################################

		## TODO: DEBUG
		elif mech_name == "GnC_thresh":
			# Thresh = mech.Thresholdout_Mechanism(hold_frac=hold_frac, threshold=threshold, sigma=sigma)
			# Thresh.add_params(beta=beta, tau=tau)
			GnC_thresh = mech.Guess_and_Check_Mechanism(mech_guess = mech.Thresholdout_Mechanism(hold_frac=hold_frac, threshold=threshold, sigma=sigma),
					check_data_frac = check_data_frac,
					use_mgf_width=False)
			GnC_thresh.add_params(beta = beta, tau = tau)
			mech_rmse = self.runs_one_mech(np.array(n_list), q_max_list, q_adapt_list, runs, GnC_thresh)
			mech_para = "GnC_thresh : HOLD_FRAC: {}, T: {}, SIGMA: {}, CHECK_FRAC: {}, BETA: {}, TAU: {}, DATA SIZE: {}, RUNS: {}".format(hold_frac, threshold, sigma, check_data_frac, beta, tau, n_list, runs)
			f.write(mech_para + ": " + str(mech_rmse) + '\n')
			for i in range(len(q_max_list)):
				querys = range(q_adapt_list[i], q_max_list[i]+1, q_adapt_list[i])
				l = min(len(mech_rmse[i]), len(querys))
				plot(querys[:l], mech_rmse[i][:l])
			return mech_rmse

	# ###################################### Guess and Check Mechanism instantiated by Data Split Mechanism: ######################################

		## TODO: DEBUG
		elif mech_name == "GnC_DataSplit":
			DataSplit = mech.Mechanism()
			DataSplit.add_params(beta=beta, tau=tau)
			GnC_DataSplit = mech.Guess_and_Check_Mechanism(mech_guess = DataSplit,
					check_data_frac = check_data_frac,
					use_mgf_width=False)
			GnC_DataSplit.add_params(beta = beta, tau = tau)
			mech_rmse = self.runs_one_mech(np.array(n_list), q_max_list, q_adapt_list, runs, GnC_DataSplit)
			mech_para = "GnC_DataSplit : BETA: {}, TAU: {}, DATA SIZE: {}, RUNS: {}".format(check_data_frac, beta, tau, n_list, runs)
			print(mech_para, mech_rmse)
			f.write(mech_para + ": " + str(mech_rmse) + '\n')
			for i in range(len(q_max_list)):
				querys = range(q_adapt_list[i], q_max_list[i]+1, q_adapt_list[i])
				l = min(len(mech_rmse[i]), len(querys))
				plot(querys[:l], mech_rmse[i][:l])
			return mech_rmse


	###################################### Guess and Check Mechanism instantiated by Baseline Mechanism: ######################################

		## TODO: DEBUG
		elif mech_name == "GnC_Baseline":
			GnC_Baseline = mech.Guess_and_Check_Mechanism(mech_guess = mech.Mechanism(),
					check_data_frac = check_data_frac,
					use_mgf_width=False)
			GnC_Baseline.add_params(beta = beta, tau = tau)
			mech_rmse = self.runs_one_mech(n_list, q_max_list, q_adapt_list, runs, GnC_Baseline)
			mech_para = "GnC_Baseline : BETA: {}, TAU: {}, DATA SIZE: {}, RUNS: {}".format(check_data_frac, beta, tau, n_list, runs)
			print(mech_para, mech_rmse)
			f.write(mech_para + ": " + str(mech_rmse) + '\n')
			for i in range(len(q_max_list)):
				querys = range(q_adapt_list[i], q_max_list[i]+1, q_adapt_list[i])
				l = min(len(mech_rmse[i]), len(querys))
				plot(querys[:l], mech_rmse[i][:l])
			return mech_rmse

	###################################### Switching Mechanisms Above ######################################
		f.close()




	def main(self):
		q_max_list = [5000]
		q_adapt_list = [200]
			
	##################################################################################################################
	###################################### Switching Mechanisms: ######################################
	##################################################################################################################
		'''
		Choose the Mechanism, Avablie mechanisms are:
		Baseline : The Empirical result on Original Data without any other operations
		DataSplit : Naive Data Splitting mechanism
		Thresh : Thresholdout mechanism from [DFHPRR'15 (NeurIPS)]
		Gauss : Adds 0-mean Gaussian noise with stddev sigma to each answer query(data), and returns answer truncated in [0.0, 1.0]
		GnC_thresh : 'Guess and Check' (GnC) query-answering mechanism instantiated by Thresholdout mechanism from [DFHPRR'15 (NeurIPS)] (3rd mechanism above)
		GnC_gauss : 'Guess and Check' (GnC) query-answering mechanism instantiated by Standard Gaussian Mechanism (4th mechanism above)
		GnC_DataSplit : 'Guess and Check' (GnC) query-answering mechanism instantiated by the Naive Data Splitting Mechanism (2nd mechanism above)
		GnC_Baseline : 'Guess and Check' (GnC) query-answering mechanism instantiated by the Empirical Query Answer Mechanism (1st mechanism above)
		'''
		mechs = ["Baseline"]
		for m in mechs:
			rmse = self.runandplot_with_one_mech(q_max_list, q_adapt_list, m)
			for i in range(len(q_max_list)):
				querys = range(q_adapt_list[i], q_max_list[i]+1, q_adapt_list[i])
				l = min(len(rmse[i]), len(querys))
				print(l, querys)
				plt.plot(querys[:l], rmse[i][:l], label= m)
		
		plt.xlabel("Queries")
		plt.ylabel("RMSE (Generalization Error) for adaptive queries")
		plt.legend()
		plt.grid()
		plt.savefig("../plots/combined-test-" + str(mechs) + ".png")
		plt.show()



	def plot_from_data(self):
		plt.figure()
		x_list = range(10, 101, 10)
		Baseline_rmse = [0.003461749999999998, 0.005167999999999998, 0.007429250000000001, 0.010434500000000003, 0.012934500000000008, 0.014819000000000004, 0.01734725, 0.019392000000000006, 0.02284150000000001, 0.02589525000000002]
		DataSplit_rmse = [0.0052704000000000015, 0.004890399999999997, 0.005069599999999993, 0.004970399999999999, 0.005001600000000001, 0.004935199999999993, 0.0049336000000000015, 0.004716799999999999, 0.004423199999999998, 0.004393599999999999]
		Thresh_rmse = [0.0016713223685691878, 0.0018763961342297397, 0.0026677859169896177, 0.0030729027122588528, 0.002635966009309241, 0.003368499118227935, 0.0035926994519737943, 0.004655638038143437, 0.004816376749581941, 0.004697146505462881]

		Gauss_rmse = [0.002079523044929074, 0.002622097092175361, 0.0030713702566929186, 0.006298445045695285, 0.0037181382490414916, 0.0027625319362159966, 0.007837710847867456, 0.005537577919863043, 0.006532317258751424, 0.004855898943437798]

		GnC_gauss_rmse = [0.01916235322528332, 0.01000000000000001, 0.009850000000000012, 0.010000000000000014, 0.008400000000000012, 0.008400000000000012, 0.007400000000000009, 0.008156000000000004, 0.008992771666666666, 0.007992771666666666]

		GnC_thresh_rmse = [0.005343771542755635, 0.006803475877977275, 0.009531920000000011, 0.009850000000000012, 0.009000000000000011, 0.00885000000000001, 0.006550000000000009, 0.00764350000000001, 0.00783484693877551, 0.008113249999999997]

		GnC_DataSplit_rmse = [0.00017313600000000033, 0.027749999999999997, 0.03300000000000001, 0.03125000000000001, 0.01675000000000001, 0.01000000000000002, 0.01000000000000002, 0.01000000000000002, 0.01000000000000002, 0.01000000000000002]

		plt.plot(x_list, Baseline_rmse, 'g', label= "empirical")
		# plt.plot(x_list, DataSplit_rmse, 'y', label= "DataSplit")
		plt.plot(x_list, Thresh_rmse, 'y', label= "Thresh")
		plt.plot(x_list, Gauss_rmse, 'r', label= "Gauss")
		# plt.plot(x_list, GnC_gauss_rmse, 'm', label= "GnC_gauss")
		# plt.plot(x_list, GnC_thresh_rmse, 'c', label= "GnC_thresh")
		# plt.plot(x_list, GnC_DataSplit_rmse, label= "GnC_DataSplit")
		plt.xlabel("Queries")
		plt.ylabel("RMSE (Generalization Error) for adaptive queries")
		plt.legend()
		plt.grid()
		plt.savefig("../plots/n_adaptivity.png")
		plt.show()


r = twoRounds()
r.runandplot_with_one_mech(q_max_list = [1000], q_adapt_list = [1000], mech_name = "DataSplit")
r.runandplot_with_one_mech(q_max_list = [1000], q_adapt_list = [1000], mech_name = "Baseline")
r.runandplot_with_one_mech(q_max_list = [1000], q_adapt_list = [1000], mech_name = "Thresh")
r.runandplot_with_one_mech(q_max_list = [1000], q_adapt_list = [1000], mech_name = "Gauss")
# r.plot_from_data()


		
