
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
x = int(sys.argv[1])


# strategy = stg.Strategy(n,  ada_freq = {"method": "additive", "method_param": q_adapt}, para=para)
DATA_SIZE = 1000
CARDINALITY = 1000
MAX_QUERY_NUM = 1000
MAX_EPOCH = 100
MEAN = 0.05
class Para:
	def __init__(self, degree = 0, coefficient = None, max_degree = 0, learning_rate = 0.1, max_iteration = MAX_EPOCH):
		self.degree = degree
		self.coefficient = coefficient if coefficient else [1.0] * max_degree
		self.max_degree = max_degree
		self.learning_rate = learning_rate
		self.max_iteration = max_iteration

def mr_single (strategy, mechanism, para = Para()):
	data_size = strategy.n
	para.degree = 0
	pre_ans = [{"para" : para}]
	k = 0
	# print("max_iter:",para.max_iteration)
	while k < para.max_iteration:
		new_coefficient = pre_ans[0]["para"].coefficient + []
		for i in range(para.max_degree):
			pre_ans[0]["para"].degree = i
			q = strategy.next_query(pre_ans)
			if q is None:
				break
			r = mechanism.get_answer(q["query"])
			if k == para.max_iteration - 2 and r[0]["answer"] is not None:
				new_coefficient[i] = new_coefficient[i] - para.learning_rate * r[0]["answer"]
			elif r[0]["answer"] is None:
				q = None
				break
		pre_ans[0]["para"].coefficient = new_coefficient
		# print(new_coefficient)
		k = k + 1


	return pre_ans[0]["para"].coefficient




def eval_mr_single(n = DATA_SIZE, cardinality = CARDINALITY, para = Para(), mechanism = mech.Mechanism()):
    strategy = stg.Strategy(n, q_mean = MEAN, ada_freq = {"method": "mr_single", "method_param": para}, q_max = MAX_QUERY_NUM, cardinality = cardinality)
    # print (strategy.cardinality)
    mechanism.reset()
    mechanism.add_data({'data': strategy.gen_data_decimal()})

    coefficient = mr_single(strategy, mechanism, para)
    
    pred_list = []
    eval_data = strategy.gen_data_decimal()
    for j in range(n):
        pred = coefficient[0]
        for i in range(1, para.max_degree):
            pred += coefficient[i] * math.pow(eval_data[j, i - 1], i)
        pred_list.append(pred)
    
    # print("eval",eval_data[:,-1])
    # print(len(eval_data))
    # print("pred",pred_list)
    mse = (np.square(np.subtract(eval_data[:, -1], pred_list)))
    # print("mse",mse)
    rmse = 	np.sqrt(mse)
    # true_data = eval_data[:,-1]
    # std = np.std(true_data)
    # amax = np.amax(true_data)
    # amin = np.amin(true_data)
    # dif= amax - amin  
    # mean = np.mean(true_data)
    # print("dif", dif)
    # print("mean", mean)
    # print("rmse",rmse)
    # nrmse = rmse/std
    # nrmse1 = rmse/dif
    # nrmse2 = rmse/mean
    # print("std", std)
    # print("nrmse",nrmse)
    # print("nrmse1", nrmse1)
    # print("nrmse2", nrmse2)
    return rmse

n = 10
cardinality = 2
para = Para(0, None, max_degree = 2, learning_rate = 0.1, max_iteration = x)
runs = 10

beta, tau = 0.05, 1.0
sigma = 0.015
hold_frac, threshold, check_data_frac = 0.7, 0.05, 0.05


Baseline = mech.Mechanism()
Baseline.add_params(beta=beta, tau=tau, check_for_width=None)
Baseline_rmse = eval_mr_single(n = n, cardinality = cardinality, para = para, mechanism = Baseline)
# print(Baseline_rmse)

DataSplit = mech.Mechanism()
DataSplit.add_params(beta=beta, tau=tau)
DataSplit_rmse = eval_mr_single(n = n, cardinality = cardinality, para = para, mechanism = DataSplit)

Thresh = mech.Thresholdout_Mechanism(hold_frac=hold_frac, threshold=threshold, sigma=sigma)
Thresh.add_params(beta=beta, tau=tau, check_for_width=None)
# Thresh_rmse = [eval_mr_single(cardinality, para, Thresh).mean() for para in stepped_para]
Thresh_rmse = eval_mr_single(n = n, cardinality = cardinality, para = para, mechanism = Thresh)


Gauss = mech.Gaussian_Mechanism(sigma=sigma)
Gauss.add_params(beta=beta, tau=tau, check_for_width=None)
# Gauss_rmse = [eval_mr_single(cardinality, para, Gauss).mean() for para in stepped_para]
Gauss_rmse = eval_mr_single(n = n, cardinality = cardinality, para = para, mechanism = Gauss)
# print(Baseline_rmse)
# print(DataSplit_rmse)
# print(Gauss_rmse)
# print(Thresh_rmse)
print(Baseline_rmse.mean(), DataSplit_rmse.mean(), Gauss_rmse.mean(), Thresh_rmse.mean())


'''
(0.7607392222855243, 0.7584168918674784, 0.5094650217969, 0.5928537773348361)
'''

'''
(0.8073530978577891, 0.5454927651049747, 0.750595997313868, 0.546063601423292)
'''