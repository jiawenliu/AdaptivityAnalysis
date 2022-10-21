# -*- coding: utf-8 -*-

from collections import OrderedDict
import cw_funcs as cw
import helper_funcs as hf
import strategies as stg
import mechanisms as mech
import numpy as np
import os
import scipy as sc


class Runner():
    """
    Base class which runs the analyst strategies. 
    Compatible with the Quandaric-Strategy and the mechanisms having fixed
    dataset size n, desired significance beta (coverage should be at least 1-beta), and tolerance width tau.
    """ 
    def __init__(self):
        self.q_cnt = 0

    def mech_create(self):

        return mech



    def one_run(self):
        for n in range(2000, 2001, 200):
            print("DATA SIZE: ", n)
            strategy = stg.Strategy(n,  ada_freq = {"method": "additive", "method_param": 100}, q_max=100)
            # gauss_mech.add_params(beta=0.5, tau=0.5, check_for_width=cw.conf_width_RZ_new)

            GnC_mech = mech.Guess_and_Check_Mechanism(mech_guess = mech.Gaussian_Mechanism(sigma=0.1),
                check_data_frac = 0.2,
                use_mgf_width=False)
            GnC_mech.add_data({'data': strategy.gen_data()})
            GnC_mech.add_params(beta=0.5, tau=0.8)

            q = strategy.next_query()
            self.q_cnt = 0

            while q:
                self.q_cnt += 1
                r = GnC_mech.get_answer(q["query"])
                # print(r)
                # print(np.array([ans["answer"] if ans["answer"] else 0 for ans in r]) - np.array(q["true_answer"]))
                if r[-1]["answer"]:
                    print("ERROR:", np.array([ans["answer"] if ans["answer"] else 0 for ans in r]) - np.array(q["true_answer"]))
                    q = strategy.next_query(r)
                else:
                    q = strategy.next_query(None)
            print(self.q_cnt)

    def main(self, n):
        for _ in range(n):
            self.one_run()


r = Runner()
r.main(1)
