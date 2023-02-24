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

class Runner():
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
		print("ANS OF ADAPTIVE QUERYS:", strategy.mech_ans_list)
		print("TRUE ANSWER OF ADAPTIVE QUERYS:", strategy.true_ans_list)
		SE = np.square(np.subtract(strategy.true_ans_list[:q_done], strategy.mech_ans_list))
		return list(SE)

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

	def runandplot_with_one_mech(self, q_max_list = [100], q_adapt_list = [10], mech_name = "DataSplit"):
		runs = 500
		n_list = [5000]
		beta, tau = 0.5, 1.0
		sigma = 0.35
		hold_frac, threshold, check_data_frac = 0.5, 0.05, 0.5


		def plot(x_list, rmse_list):
			plt.plot(x_list, rmse_list, 'r', label= mech_name)
			plt.xlabel("Queries")
			plt.ylabel("RMSE (Generalization Error) for adaptive queries")
			plt.legend()
			plt.grid()
			plt.savefig("../plots/" + mech_name + "-tuning.png")
			plt.show()

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
		f = open('../results/'+ mech_name + "-tuning.txt", 'w')
	# ###################################### Emperical Result: ######################################
		if mech_name == "Baseline":
			Baseline = mech.Mechanism()
			Baseline.add_params(beta=beta, tau=tau, check_for_width=None)
			Baseline_rmse = self.runs_one_mech(n_list, q_max_list, q_adapt_list, runs, Baseline)
			mech_para = mech_name
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
			mech_rmse = self.runs_one_mech(n_list, q_max_list, q_adapt_list, runs, DataSplit)
			mech_para = "DATA SPLIT : BETA: {}, TAU: {}".format(beta, tau)
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
			mech_para = "THRESH : HOLD_FRAC: {}, T: {}, SIGMA: {}, BETA: {}, TAU: {}".format(hold_frac, threshold, sigma, beta, tau)
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
			mech_para = "GAUSS : SIGMA: {}, BETA: {}, TAU: {}".format(sigma, beta, tau)
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
			GnC_gauss = mech.Guess_and_Check_Mechanism(mech_guess = mech.Gaussian_Mechanism(sigma=sigma),
				check_data_frac = check_data_frac,
				use_mgf_width=False)
			GnC_gauss.add_params(beta = beta, tau = tau)
			mech_rmse = self.runs_one_mech(np.array(n_list), q_max_list, q_adapt_list, runs, GnC_gauss)
			mech_para = "GnC_gauss : SIGMA: {}, CHECK_FRAC: {}, BETA: {}, TAU: {}".format(sigma, check_data_frac, beta, tau)
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
			GnC_thresh = mech.Guess_and_Check_Mechanism(mech_guess = mech.Thresholdout_Mechanism(hold_frac=hold_frac, threshold=threshold, sigma=sigma),
					check_data_frac = check_data_frac,
					use_mgf_width=False)
			GnC_thresh.add_params(beta = beta, tau = tau)
			mech_rmse = self.runs_one_mech(np.array(n_list), q_max_list, q_adapt_list, runs, GnC_thresh)
			mech_para = "GnC_thresh : HOLD_FRAC: {}, T: {}, SIGMA: {}, CHECK_FRAC: {}, BETA: {}, TAU: {}".format(hold_frac, threshold, sigma, check_data_frac, beta, tau)
			f.write(mech_para + ": " + str(mech_rmse) + '\n')
			for i in range(len(q_max_list)):
				querys = range(q_adapt_list[i], q_max_list[i]+1, q_adapt_list[i])
				l = min(len(mech_rmse[i]), len(querys))
				plot(querys[:l], mech_rmse[i][:l])
			return mech_rmse

	# ###################################### Guess and Check Mechanism instantiated by Data Split Mechanism: ######################################

		## TODO: DEBUG
		elif mech_name == "GnC_DataSplit":
			GnC_DataSplit = mech.Guess_and_Check_Mechanism(mech_guess = mech.Mechanism(),
					check_data_frac = check_data_frac,
					use_mgf_width=False)
			GnC_DataSplit.add_params(beta = beta, tau = tau)
			mech_rmse = self.runs_one_mech(np.array(n_list), q_max_list, q_adapt_list, runs, GnC_DataSplit)
			mech_para = "GnC_DataSplit : BETA: {}, TAU: {}".format(check_data_frac, beta, tau)
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
			mech_para = "GnC_Baseline : BETA: {}, TAU: {}".format(check_data_frac, beta, tau)
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
		# Baseline_rmse = [0.0001665159374999998, 0.000280105625, 0.00041475312499999984, 0.0005399828125000001, 0.0006480537500000003, 0.0007589406249999998, 0.0008813671875000004, 0.0010082128125, 0.0011573356249999995, 0.0012310365625000002, 0.0013951171875000007, 0.0014977365624999992]
		# DataSplit_rmse = [0.0003032670242514105, 0.00028734488351958944, 0.0005059011434087313, 0.0005188378827533361, 0.00033983819124481276, 0.00037927036790096443, 0.00025836161171869553, 0.0003782752341052257, 0.00025512742688254435, 0.0005789190856710687, 0.0005153549144682502, 0.00015486769696185655]
		# Gauss_rmse = [0.00019156843231417413, 0.0002510482313358441, 0.0003553804321045485, 0.0004019242921611544, 0.0005395197780886264, 0.0005908343520085124, 0.0006706263589167271, 0.0007110070961447595, 0.0008281670129612613, 0.000907698461997637, 0.0009774114254126954, 0.0010208326769023958]
		# Thresh_rmse = [0.00011515008744002371, 0.000192084826585693, 0.00034196007219071075, 0.0003588871432375274, 0.0004630447258813043, 0.0005224579960931553, 0.0003508284657343754, 0.0007192016398570339, 0.0008024494073815844, 0.0008019615961571019, 0.0008349052712272731, 0.0012759390682007036]
		# GnC_gauss_rmse = [ 0.007250000000000001, 0.008000000000000002,0.004976787946650419, 0.007100000000000008, 0.0019999999999999987, 0.0018000000000000032, 0.0016000000000000038, 0.0016000000000000038, 0.0010400000000000006, 0.0010400000000000006, 0.0016000000000000038, 0.0016000000000000038]
		# GnC_thresh_rmse = [ 0.006100000000000003, 0.006400000000000003, 0.006400000000000003, 0.006400000000000003, 0.0028000000000000034, 0.0016000000000000035, 0.0016000000000000035, 0.0004000000000000009, 0.00040000000000000083, 0.0016000000000000035, 0.0016000000000000035, 0.0016000000000000035]
		# GnC_DataSplit_rmse = [0.001600000000000003, 0.0004000000000000007, 0.0004000000000000007, 0.0004000000000000007, 0.0016000000000000035, 0.0016000000000000035, 0.0016000000000000035, 0.0004000000000000007, 0.0004000000000000007, 0.0016000000000000035, 0.0016000000000000035, 0.0016000000000000035]
		Baseline_rmse = [0.003461749999999998, 0.005167999999999998, 0.007429250000000001, 0.010434500000000003, 0.012934500000000008, 0.014819000000000004, 0.01734725, 0.019392000000000006, 0.02284150000000001, 0.02589525000000002]
		DataSplit_rmse = [0.0052704000000000015, 0.004890399999999997, 0.005069599999999993, 0.004970399999999999, 0.005001600000000001, 0.004935199999999993, 0.0049336000000000015, 0.004716799999999999, 0.004423199999999998, 0.004393599999999999]
		Thresh_rmse = [0.0016713223685691878, 0.0018763961342297397, 0.0026677859169896177, 0.0030729027122588528, 0.002635966009309241, 0.003368499118227935, 0.0035926994519737943, 0.004655638038143437, 0.004816376749581941, 0.004697146505462881]

		Gauss_rmse = [0.002079523044929074, 0.002622097092175361, 0.0030713702566929186, 0.006298445045695285, 0.0037181382490414916, 0.0027625319362159966, 0.007837710847867456, 0.005537577919863043, 0.006532317258751424, 0.004855898943437798]

		GnC_gauss_rmse = [0.01916235322528332, 0.01000000000000001, 0.009850000000000012, 0.010000000000000014, 0.008400000000000012, 0.008400000000000012, 0.007400000000000009, 0.008156000000000004, 0.008992771666666666, 0.007992771666666666]

		GnC_thresh_rmse = [0.005343771542755635, 0.006803475877977275, 0.009531920000000011, 0.009850000000000012, 0.009000000000000011, 0.00885000000000001, 0.006550000000000009, 0.00764350000000001, 0.00783484693877551, 0.008113249999999997]

		GnC_DataSplit_rmse = [0.00017313600000000033, 0.027749999999999997, 0.03300000000000001, 0.03125000000000001, 0.01675000000000001, 0.01000000000000002, 0.01000000000000002, 0.01000000000000002, 0.01000000000000002, 0.01000000000000002]

		plt.plot(x_list, Baseline_rmse, 'r', label= "Baseline")
		plt.plot(x_list, DataSplit_rmse, 'y', label= "DataSplit")
		plt.plot(x_list, Thresh_rmse, 'g', label= "Thresh")
		plt.plot(x_list, Gauss_rmse, 'b', label= "Gauss")
		plt.plot(x_list, GnC_gauss_rmse, 'm', label= "GnC_gauss")
		plt.plot(x_list, GnC_thresh_rmse, 'c', label= "GnC_thresh")
		plt.plot(x_list, GnC_DataSplit_rmse, label= "GnC_DataSplit")
		plt.xlabel("Queries")
		plt.ylabel("RMSE (Generalization Error) for adaptive queries")
		plt.legend()
		plt.grid()
		plt.savefig("../plots/combined-tuning2.png")
		plt.show()


r = Runner()
# r.runandplot_with_one_mech()
r.plot_from_data()


		
