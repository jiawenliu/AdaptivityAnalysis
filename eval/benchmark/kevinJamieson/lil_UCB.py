
import math
import numpy as np

from enum import Enum


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
