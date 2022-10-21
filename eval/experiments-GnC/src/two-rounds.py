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


class Runner():
    """
    Base class which runs the analyst strategies. 
    Compatible with the Quandaric-Strategy and the mechanisms having fixed
    dataset size n, desired significance beta (coverage should be at least 1-beta), and tolerance width tau.
    """ 
    def __init__(self):
        self.q_cnt = 0

    def one_run(self, n):
            strategy = stg.Strategy(n,  ada_freq = {"method": "additive", "method_param": 20}, q_max=100)
            
##################################################################################################################
###################################### Switching Mechanisms: ######################################
##################################################################################################################

# ###################################### Emperical Result: ######################################

            # mechanism = mech.Mechanism()
            # mechanism.add_params(beta=0.5, tau=0.5, check_for_width=None)

# ###################################### Data Splitting Mechanism: ######################################

            # mechanism = mech.Mechanism()
            # mechanism.add_params(beta=0.1, tau=0.1)

# ###################################### Thresholdout Mechanism: ######################################

            # mechanism = mech.Thresholdout_Mechanism(hold_frac=0.4, threshold=0.01, sigma=0.1)
            # mechanism.add_params(beta=0.5, tau=0.5, check_for_width=None)

# ###################################### Gaussian Mechanism: ######################################

            # mechanism = mech.Gaussian_Mechanism(sigma=0.03)
            # mechanism.add_params(beta=0.5, tau=0.5, check_for_width=None)

# ###################################### Guess and Check Mechanism instantiated by Gaussian Mechanism: ######################################

            mechanism = mech.Guess_and_Check_Mechanism(mech_guess = mech.Gaussian_Mechanism(sigma=0.03),
                check_data_frac = 0.3,
                use_mgf_width=False)
            mechanism.add_params(beta=0.5, tau=0.5)

# ###################################### Guess and Check Mechanism instantiated by Thresholdout Mechanism: ######################################
 
            # mechanism = mech.Guess_and_Check_Mechanism(mech_guess = mech.Thresholdout_Mechanism(hold_frac=0.2, threshold=0.01, sigma=0.1),
            #     check_data_frac = 0.3,
            #     use_mgf_width=False)
            # mechanism.add_params(beta=0.5, tau=0.5)

###################################### Switching Mechanisms Above ######################################

          
            mechanism.add_data({'data': strategy.gen_data()})
            

            q = strategy.next_query()
            while q:
                r = mechanism.get_answer(q["query"])
                if r[-1]["answer"] is not None:
                    q = strategy.next_query(r)
                else:
                    q = None
            q_done = len(strategy.mech_ans_list)
            print("ANS OF ADAPTIVE QUERYS:", strategy.mech_ans_list)
            print("TRUE ANSWER OF ADAPTIVE QUERYS:", strategy.true_ans_list)
            print("ERROR OF EACH ADAPTIVE QUERY:", np.array(strategy.true_ans_list[:q_done]) - np.array(strategy.mech_ans_list))
            MSE = np.square(np.subtract(strategy.true_ans_list[:q_done], strategy.mech_ans_list)).mean()
            print("MSE:", MSE)
            print("ME:", np.sum((np.array(strategy.true_ans_list[:q_done]) - np.array(strategy.mech_ans_list)))/len(strategy.mech_ans_list))
            return MSE

    def main(self, runs = 1):
        for n in range(1000, 1001):
            print("RMSE:", sum([math.sqrt(self.one_run(n)) for _ in range(runs)])/runs)


r = Runner()
r.main(runs = 50)
