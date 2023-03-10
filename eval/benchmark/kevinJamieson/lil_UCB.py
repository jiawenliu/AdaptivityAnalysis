
import math
import numpy as np

from enum import Enum


def sparse_borda (k, n, time_gate):
	arm_candidates  = range(n)
	while len(arm_candidates) > 1:
		for arm in arm_candidates:
			index = np.random.choice(arm_candidates)
			observation = np.random.binomial(p)
			p = update_bias(p, index)

		arm_candidates


	pass