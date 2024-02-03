
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
MAX_EPOCH = 1000
MEAN = 0.1
class Para:
	def __init__(self, degree = 0, coefficient = None, max_degree = 0, learning_rate = 0.1, max_iteration = MAX_EPOCH):
		self.degree = degree
		self.coefficient = coefficient if coefficient else [0.0] * max_degree
		self.max_degree = max_degree
		self.learning_rate = learning_rate
		self.max_iteration = max_iteration

def lrgd (strategy, mechanism, para = Para()):
	para.degree = 0
	pre_ans = [{"para" : para}]
	k = 0
	while k < para.max_iteration:
		new_coefficient = pre_ans[0]["para"].coefficient + []
		for i in range(para.max_degree):
			pre_ans[0]["para"].degree = i
			q = strategy.next_query(pre_ans)
			if q is None:
				break
			r = mechanism.get_answer(q["query"])
			if r[0]["answer"] is not None:
				new_coefficient[i] = new_coefficient[i] - (-2) * para.learning_rate * r[0]["answer"]
			else:
				q = None
				break
		pre_ans[0]["para"].coefficient = new_coefficient
		k += 1

	return pre_ans[0]["para"].coefficient





def eval_lrgd(n = DATA_SIZE, cardinality = CARDINALITY, mechanism = mech.Mechanism()):
    para = Para(1, None, max_degree = cardinality, learning_rate = 0.5, max_iteration = 10)
    strategy = stg.Strategy(n, q_mean = MEAN, ada_freq = {"method": "n_dim_lrgd", "method_param": para}, q_max = MAX_QUERY_NUM, cardinality = cardinality)
    mechanism.reset()
    mechanism.add_data({'data': strategy.gen_data_decimal()})
    # print(mechanism.data)
    coefficient = lrgd(strategy, mechanism, para)
    
    pred_list, eval_size = [], n
    eval_data = mechanism.data
    
    for j in range(eval_size):
        pred = para.coefficient[0]
        for i in range(1, cardinality):
            pred += math.pow(eval_data[j, i-1], i) * para.coefficient[i]
        pred_list.append(pred)
    pred_list = np.sign(pred_list)
    mse = (np.square(np.subtract(eval_data[:, -1], pred_list)))
    rmse = np.sqrt(mse)
    true_data = eval_data[:,-1]
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
 
    # pred_list = np.sign(pred_list)
    # mse = (np.square(np.subtract(eval_data[:, -1], pred_list)))
    
    # return np.sqrt(mse)
 
n = 1000
cardinality = 3
max_iteration = 10

beta, tau = 0.05, 1.0
sigma = 0.0035
# delta = 0.1
hold_frac, threshold, check_data_frac, delta = 0.5, 0.05, 0.05, 0.05

runs = range(100)

Baseline = mech.Mechanism()
Baseline.add_params(beta=beta, tau=tau, check_for_width=None)
Baseline_rmse = eval_lrgd(n = n, cardinality = cardinality, mechanism = Baseline)
Baseline_rmse = np.array([eval_lrgd(n = n, cardinality = cardinality, mechanism = Baseline).mean() for _ in runs])


DataSplit = mech.Mechanism(max_q = max_iteration)
DataSplit.add_params(beta=beta, tau=tau)
DataSplit_rmse = eval_lrgd(n = n * n, cardinality = cardinality, mechanism = DataSplit)
DataSplit_rmse = np.array([eval_lrgd(n = n * n, cardinality = cardinality, mechanism = DataSplit).mean() for _ in runs])


Thresh = mech.Thresholdout_Mechanism(hold_frac=hold_frac, threshold=threshold, sigma=delta)
Thresh.add_params(beta=beta, tau=tau, check_for_width=None)
# Thresh_rmse = [eval_lrgd(cardinality, para, Thresh).mean() for para in stepped_para]
Thresh_rmse = eval_lrgd(n = n, cardinality = cardinality, mechanism = Thresh)
Thresh_rmse = np.array([eval_lrgd(n = n, cardinality = cardinality, mechanism = Thresh).mean() for _ in runs])


Gauss = mech.Gaussian_Mechanism(sigma=sigma)
Gauss.add_params(beta=beta, tau=tau, check_for_width=None)
# Gauss_rmse = [eval_lrgd(cardinality, para, Gauss).mean() for para in stepped_para]
Gauss_rmse = eval_lrgd(n = n, cardinality = cardinality, mechanism = Gauss)
Gauss_rmse = np.array([eval_lrgd(n = n, cardinality = cardinality, mechanism = Gauss).mean() for _ in runs])

print(Baseline_rmse.mean(), DataSplit_rmse.mean(), Gauss_rmse.mean(), Thresh_rmse.mean())

'''
(0.7607392222855243, 0.7584168918674784, 0.5094650217969, 0.5928537773348361)
'''

'''
(0.2456989012034612, 0.7787739611838521, 0.2607422786977378, 0.22452097420785816)
'''

'''
Degree 2:
(0.12, 0.11600000000000002, 0.1, 0.06000000000000001)
'''

'''
Degree 3:
(1.7208879432323872e+52, 414.5756662924357, 2.2250276552173854e+50, 72.34362425773182)
'''

'''
(0.11599999999999999, 0.1, 0.10799999999999998, 0.092)
'''

'''
Degree 4:
(7.26581290763821e+103, 21947.169727467335, 7.489180459211598e+102, 5179956471897.063)
'''


'''
(0.11120000000000001, 0.10548492000000001, 0.096, 0.1092)'''

'''
Degree 4:
(0.6, 0.10400000000000001, 0.3, 0.13999999999999999)
'''