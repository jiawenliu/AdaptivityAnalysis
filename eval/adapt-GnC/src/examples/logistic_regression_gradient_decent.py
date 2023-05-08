
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
				print(r[0]["answer"])
				new_coefficient[i] = new_coefficient[i] - (-2) * para.learning_rate * r[0]["answer"]
			else:
				q = None
				break
		pre_ans[0]["para"].coefficient = new_coefficient
		print(pre_ans[0]["para"].coefficient)
		k += 1

	return pre_ans[0]["para"].coefficient





def eval_lrgd(n = DATA_SIZE, cardinality = CARDINALITY, mechanism = mech.Mechanism()):
    para = Para(0, None, max_degree = cardinality, learning_rate = 0.5, max_iteration = 1000)
    strategy = stg.Strategy(n, q_mean = MEAN, ada_freq = {"method": "mr_odd", "method_param": para}, q_max = MAX_QUERY_NUM, cardinality = cardinality)
    mechanism.reset()
    mechanism.add_data({'data': strategy.gen_data_decimal()})
    coefficient = lrgd(strategy, mechanism, para)
    
    pred_list, eval_size = [], 1000
    strategy.n = eval_size
    eval_data = strategy.gen_data_decimal()
    
    for j in range(eval_size):
        pred = para.coefficient[0]
        for i in range(1, cardinality):
            pred += math.pow(eval_data[j, i-1], i) * para.coefficient[i]
        pred_list.append(pred)
    
    mse = (np.square(np.subtract(eval_data[:, -1], pred_list)))
    
    return np.sqrt(mse)
 
n = 100
cardinality = 4
max_iteration = 50

beta, tau = 0.05, 1.0
sigma = 0.35
hold_frac, threshold, check_data_frac = 0.7, 0.05, 0.05

runs = range(10)

Baseline = mech.Mechanism()
Baseline.add_params(beta=beta, tau=tau, check_for_width=None)
Baseline_rmse = eval_lrgd(n = n, cardinality = cardinality, mechanism = Baseline)
Baseline_rmse = np.array([eval_lrgd(n = n, cardinality = cardinality, mechanism = Baseline).mean() for _ in runs])


DataSplit = mech.Mechanism(max_q = max_iteration)
DataSplit.add_params(beta=beta, tau=tau)
DataSplit_rmse = eval_lrgd(n = n, cardinality = cardinality, mechanism = DataSplit)
DataSplit_rmse = np.array([eval_lrgd(n = n, cardinality = cardinality, mechanism = DataSplit).mean() for _ in runs])


Thresh = mech.Thresholdout_Mechanism(hold_frac=hold_frac, threshold=threshold, sigma=sigma)
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
Degree 3:
(1.7208879432323872e+52, 414.5756662924357, 2.2250276552173854e+50, 72.34362425773182)
'''


'''
Degree 4:
(8.728997306256985e+106, 24311.208107266462, 2.742733773103375e+102, 7528526.137190346)
'''