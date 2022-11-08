# -*- coding: utf-8 -*-

from collections import OrderedDict
import cw_funcs as cw
import helper_funcs as hf
import numpy as np
import os
import scipy as sc


class Mechanism:
    """
    Base class which defines the structure for query-answering mechanisms having fixed dataset size n,
    desired significance beta (coverage should be at least 1-beta), and tolerance width tau.
    """
    def __init__(self):
        """
        Initializer for the mechanism.
        """
        self.name = "Empirical"

        self.data = None
        self.n = None
        self.curr_q = 0
        self.beta = None
        self.tau = None
        self.max_q = None
        self.samp_per_q = None
        self.check_for_width = None

    def reset(self):
        self.data = None
        self.n = None
        self.curr_q = 0
        self.max_q = None
        self.samp_per_q = None


    def get_worst_case_bound(self, check_for_width_dict, tau):
        """
        Computes the maximum number of queries that can be answered by the mechanism (in the worst case) with the given
        accuracy parameters.
        :param check_for_width_dict: dict containing parameters (except number of queries) for getting valid widths
                                     via function check_for_width
        :param tau: width of the required certified confidence interval
        :returns: a number > 0 denoting the maximum number of queries answerable by the mechanism with the given
                  accuracy parameters
        """

        # Determine how large the upper bound can be, so we can search within a finite range
        q_low = 1
        q_high = 2
        check_for_width_dict['q'] = q_high
        cur_tau = self.check_for_width(**check_for_width_dict)  # certifiable width for q_high queries
        while tau >= cur_tau > 0.0:
            q_low = q_high
            q_high *= 2
            check_for_width_dict['q'] = q_high
            cur_tau = self.check_for_width(**check_for_width_dict)

        # Use binary search to find the highest value of q in [q_low, q_high] such that cur_tau <= tau
        while q_high > q_low + 1:
            cur_q = q_low + int((q_high - q_low) / 2)
            check_for_width_dict['q'] = cur_q
            cur_tau = self.check_for_width(**check_for_width_dict)
            if cur_tau > tau:
                q_high = cur_q
            else:
                q_low = cur_q
        return q_low

    def add_data(self, data_dict):
        """
        Adds data to the mechanism.
        :param data_dict: should be a dict containing a key "data" with value being a matrix, where each row in
                          the matrix represents a sample of the population. Can also contain a key "name" which
                          contains the name of the generated dataset (the same dataset will be generated for the same
                          name in the future)
        """

        self.data = data_dict["data"]
        self.n = len(self.data)
        if "name" in data_dict:
            hf.initialize_with_str_seed(data_dict["name"])

    def add_params(self, beta, tau, check_for_width=cw.chernoff_conf):
        """
        Adds accuracy parameters (tau, beta), and function for computing valid widths, to the mechanism.
        :param beta: desired significance (coverage should be at least 1-beta)
        :param tau: width of the certified confidence interval
        :param check_for_width: function for computing valid confidence widths for the mechanism.
                                Default = function that computes valid widths via Chernoff bound
        """
        self.beta = beta
        self.tau = tau
        if check_for_width is not None:
            self.check_for_width = check_for_width

    def get_answer(self, query):
        """
        Computes the answer for the given query.
        :param query: function that takes in a dataset, and returns a list of value(s)
        :returns: a dictionary, with a key "answer" containing the empirical answer(s) on the complete dataset if
                  self.check_for_width is None, otherwise it uses Naive Data Splitting to answer each query. "answer"
                  is None if max. possible queries have been already asked to the mechanism.
        """

        assert self.n is not None, "No data added. Add data by calling the function add_data."

        if self.check_for_width is None:  # If no function input to certify widths, return empirical answer.
            ans = query(self.data)
            self.curr_q += len(ans)
            ans_list = [{"answer": ans[i]} for i in range(len(ans))]
        else:
            if self.max_q is None:
                self.name = "Naive Data Split"
                assert self.beta is not None and self.tau is not None, "No params added. Add params by calling " \
                                                                       "the function add_params."
###################################### Original Code: ######################################

                # self.max_q = self.get_worst_case_bound({'n': self.n, 'beta': self.beta}, self.tau)
                # print("Max Query #:", self.max_q)
                # self.samp_per_q = int(np.ceil(self.n / self.max_q))  # number of samples available for each query
                # print("Splitted Sample Size:", self.samp_per_q)

###################################### Original Code ^^ ######################################

###################################### Debugging Code: ######################################
                
                self.max_q = len(self.data[0])
                print("Max Query #:", self.max_q)
                self.samp_per_q = int((self.n / (self.max_q - 1)))  # number of samples available for each query
                # self.samp_per_q = int((self.n / 2))  # number of samples available for each query
                print("Splitted Sample Size:", self.samp_per_q)

###################################### Debugging Code ^^ ######################################

###################################### Original Code: ######################################

# The original code isn't compatible with the strategy.

            # if self.curr_q < self.max_q:
            #     ans = query(self.data[self.curr_q * self.samp_per_q: (self.curr_q + 1) * self.samp_per_q])
            #     ans_list = []
            #     num_queries = len(ans)
            #     for i in range(min(num_queries, self.max_q - self.curr_q)):  # Continue until current batch of queries,
            #         # or until max. possible queries are answered
            #         ans_list.append({"answer": ans[i]})
            #         self.curr_q += 1
            #         ans = query(self.data[self.curr_q * self.samp_per_q: (self.curr_q + 1) * self.samp_per_q])

###################################### Original Code ^^ ######################################

###################################### Debugging Code 3: ######################################

            # if self.curr_q < self.max_q:
            #     ans = query(self.data[self.curr_q * self.samp_per_q: (self.curr_q + 1) * self.samp_per_q])
            #     self.curr_q += len(ans)
            #     ans_list = [{"answer": ans[i]} for i in range(len(ans))]


###################################### Debugging Code 2: ######################################
                
            if self.curr_q < self.max_q:
                ans = query(self.data[self.curr_q * self.samp_per_q: (self.curr_q + 1) * self.samp_per_q])
                ans_list = []
                num_queries = min(len(ans), self.max_q - self.curr_q)
                # print("SIZE OF DATA FOR 1 BATCH", num_queries * self.samp_per_q)
                ans = query(self.data[self.curr_q * self.samp_per_q: (self.curr_q + num_queries) * self.samp_per_q])
                self.curr_q += len(ans)
                ans_list = [{"answer": ans[i]} for i in range(len(ans))]
                

                # for i in range(num_queries):  # Continue until current batch of queries,
                #     # or until max. possible queries are answered
                #     ans_list.append({"answer": ans[i]})
                #     self.curr_q += 1
                #     ans = query(self.data[self.curr_q * self.samp_per_q: (self.curr_q + 1) * self.samp_per_q])
###################################### Debugging Code ^^ ######################################
            else:
                return [{"answer": None}]
        return ans_list


class Gaussian_Mechanism(Mechanism):
    """
    Adds 0-mean Gaussian noise with stddev sigma to each answer query(data), and returns answer truncated in [0.0, 1.0].
    """
    def __init__(self, sigma):
        """
        Initializer for the Gaussian mechanism.
        :param sigma: standard deviation of the 0-mean Gaussian noise added to each query answer
        """
        Mechanism.__init__(self)
        self.sigma = sigma
        self.name = "Gauss"

    def add_params(self, beta, tau, check_for_width=cw.conf_width_RZ_new):
        """
        Adds accuracy parameters (tau, beta), and function for computing valid widths, to the mechanism.
        :param beta: desired significance (coverage should be at least 1-beta)
        :param tau: width of the certified confidence interval
        :param check_for_width: function for computing valid confidence widths for the mechanism.
                                Default = function that computes valid widths via CDP [BS'16] +
                                          the Monitor argument [BNSSSU'16] + [RZ'16]
        """
        Mechanism.add_params(self, beta, tau, check_for_width=check_for_width)

    def get_answer(self, query):
        """
        Computes the answer for the given query.
        :param query: function that takes in a dataset, and returns a list of value(s)
        :returns: a dictionary, with a key "answer" containing the empirical answer(s) on the complete dataset with
                  Gaussian noise added. If self.check_for_width is given, then "answer" is None if max. possible
                  queries have been already asked to the mechanism.
        """

        assert self.n is not None, "No data added. Add data by calling the function add_data."

        ans = query(self.data)
        self.curr_q += len(ans)
        noise_to_add = np.random.normal(0, scale=self.sigma, size=len(ans))
        omit_ans = 0

        if self.check_for_width is not None:
            if self.max_q is None:
                assert self.beta is not None and self.tau is not None, "No params added. Add params by calling " \
                                                                       "the function add_params."
                if self.sigma == -1.0:  # calculated using optimal value of sigma according to the result in the paper
                    self.max_q = self.get_worst_case_bound({'n': self.n, 'beta': self.beta}, self.tau)
                else:
                    self.max_q = self.get_worst_case_bound({'n': self.n, 'beta': self.beta, 'sigma': self.sigma},
                                                           self.tau)

            if self.curr_q < self.max_q:
                omit_ans = max(0, self.curr_q - self.max_q)
            else:
                return [{"answer": None}]
        # print("GAUSS")

        return [{"answer": min(1.0, max(0.0, ans[i] + noise_to_add[i]))} for i in range(len(ans) - omit_ans)]


class Thresholdout_Mechanism(Mechanism):
    """
    Implementation of the Thresholdout mechanism from [DFHPRR'15 (NeurIPS)].
    """
    def __init__(self, hold_frac, threshold, sigma):
        """
        Initializer for Thresholdout.
        :param hold_frac: data fraction to be assigned as holdout set.
        :param threshold: threshold for comparing training and holdout answers
        :param sigma: noise parameter for Laplace noise added to the comparisons
        """
        Mechanism.__init__(self)
        self.hold_size = None
        self.train_size = None
        assert 0.0 < hold_frac <= 1.0, "hold_frac should take a value in (0, 1]."
        self.hold_frac = hold_frac
        self.threshold = threshold
        self.sigma = sigma
        self.name = "Thresh"
        self.noisy_thresh = self.threshold + np.random.laplace(0, 2 * self.sigma)
        self.n = None
        self.cost_q = 0

    def add_params(self, beta, tau, check_for_width=cw.bounds_Thresh):
        """
        Adds accuracy parameters (tau, beta), and function for computing valid widths, to the mechanism.
        :param beta: desired significance (coverage should be at least 1-beta)
        :param tau: width of the certified confidence interval
        :param check_for_width: function for computing valid confidence widths for the mechanism.
                                Default = function from the paper that computes valid widths for Thresholdout
        """
        Mechanism.add_params(self, beta, tau, check_for_width=check_for_width)

    def get_answer(self, query):
        """
        Computes the answer for the given query.
        :param query: function that takes in a dataset, and returns a list of value(s)
        :returns: a dictionary, with a key "answer" containing the answer(s) on the dataset via Thresholdout.
                  If self.check_for_width is given, then "answer" is None if max. possible
                  queries have been already asked to the mechanism.
        """

        assert self.n is not None, "No data added. Add data by calling the function add_data."

        if self.train_size is None:
            self.hold_size = int(np.floor(self.n * self.hold_frac))
            self.train_size = self.n - self.hold_size
            assert self.train_size >= 1, "Training set size should be at least 1."

        ans_list = []
        train_ans = query(self.data[: self.train_size])
        hold_ans = query(self.data[self.train_size: self.n])
        assert len(train_ans) == len(hold_ans), "Training and Holdout answers should have same length."
        for i in range(len(train_ans)):
            self.curr_q += 1
            if np.abs(train_ans[i] - hold_ans[i]) <= self.noisy_thresh + np.random.laplace(0, 4 * self.sigma):
                ans = train_ans[i]
            else:
                self.cost_q += 1
                self.noisy_thresh = self.threshold + np.random.laplace(0, 2 * self.sigma)
                ans = min(1.0, max(0.0, hold_ans[i] + np.random.laplace(0, self.sigma)))

###################################### Debugging Code: ######################################

            # check = self.check_for_width(h=self.hold_size, thresh=self.threshold, 
            #             q=self.curr_q, 
            #             b=self.cost_q,
            #             sigma=self.sigma, beta=self.beta)
            # print("Check For Width Result: ", check)
            # print("Answer Before Check: ", ans)

            # if (self.check_for_width is None or
            #         check <= self.tau):

###################################### Debugging Substitution: ######################################

            if (self.check_for_width is None or
                    self.check_for_width(h=self.hold_size, thresh=self.threshold, 
                        q=self.curr_q, 
                        b=self.cost_q,
                        sigma=self.sigma, beta=self.beta) <= self.tau):
                ans_list.append({"answer": ans})

###################################### Debugging Code ######################################

            else:
                ans_list.append({"answer": None})
                break
        # print("THRESHOLDOUT")
        return ans_list


class Guess_and_Check_Mechanism:
    """
    Base class which defines the structure for the 'Guess and Check' (GnC) query-answering mechanism.
    """

    def __init__(self, mech_guess, check_data_frac, use_mgf_width=True, **kwargs):
        """
        Initializer for the GnC mechanism.
        :param mech_guess: guess mechanism that will be used for taking guesses to queries
        :param check_data_frac: data fraction to be assigned as holdout set
        :param use_mgf_width: if True, tolerance for holdout is computed via mgf_width function, else via
                              the Chernoff bound
        """
        assert 0.0 < check_data_frac < 1.0, "Check mechanism's data fraction should be in (0, 1)."
        self.M_guess = mech_guess
        self.check_data_frac = check_data_frac
        self.use_mgf_width = use_mgf_width

        # Determine if tau needs to be increased at every failure.
        self.start_tau = (0.0 if ("multiply_tau" not in kwargs or kwargs["multiply_tau"] is None or
                                  "start_tau" not in kwargs["multiply_tau"])
                          else kwargs["multiply_tau"]["start_tau"])  # Start value of tau. Default = 0.0
        self.max_tau = (0.5 if ("multiply_tau" not in kwargs or kwargs["multiply_tau"] is None or
                                "max_tau" not in kwargs["multiply_tau"])
                        else kwargs["multiply_tau"]["max_tau"])  # Max. value of tau. Default = 0.5
        self.tau_multiplier = (1.0 if ("multiply_tau" not in kwargs or kwargs["multiply_tau"] is None or
                                       "tau_multiplier" not in kwargs["multiply_tau"])
                               else kwargs["multiply_tau"]["tau_multiplier"])  # Multiplication factor for tau at
        # every failure. Default = 1.0
        self.q_encoding_param = -3 if "q_encoding_param" not in kwargs else kwargs["q_encoding_param"]  # If provided,
        # use kwargs["q_encoding_param"] for calculating terms for union bound over number of queries.
        # Default = -2 (inverse quadratic series)
        self.f_encoding_param = -3 if "f_encoding_param" not in kwargs else kwargs["f_encoding_param"]   # If provided,
        # use power kwargs["f_encoding_param"] for calculating terms for union bound over number of failures.
        # Default = -2 (inverse quadratic series)



        ###################################### Debugging Code: ######################################
        '''
        Testing Different default value for parameters
        '''
        self.start_tau = (0.0 if ("multiply_tau" not in kwargs or kwargs["multiply_tau"] is None or
                                  "start_tau" not in kwargs["multiply_tau"])
                          else kwargs["multiply_tau"]["start_tau"])  # Start value of tau. Default = 0.0
        self.max_tau = (1.0 if ("multiply_tau" not in kwargs or kwargs["multiply_tau"] is None or
                                "max_tau" not in kwargs["multiply_tau"])
                        else kwargs["multiply_tau"]["max_tau"])  # Max. value of tau. Default = 0.5
        self.tau_multiplier = (1.0 if ("multiply_tau" not in kwargs or kwargs["multiply_tau"] is None or
                                       "tau_multiplier" not in kwargs["multiply_tau"])
                               else kwargs["multiply_tau"]["tau_multiplier"])  # Multiplication factor for tau at
        # every failure. Default = 1.0

        self.q_encoding_param = -3 if "q_encoding_param" not in kwargs else kwargs["q_encoding_param"]  # If provided,
        # use kwargs["q_encoding_param"] for calculating terms for union bound over number of queries.
        # Default = -2 (inverse quadratic series)
        self.f_encoding_param = -3 if "f_encoding_param" not in kwargs else kwargs["f_encoding_param"]   # If provided,
        # use power kwargs["f_encoding_param"] for calculating terms for union bound over number of failures.
        # Default = -2 (inverse quadratic series)
    
    ###################################### Debugging Code: ######################################


        self.check_data = None
        self.check_n = None
        self.gamma_list = None  # To store the discretization parameter gamma used at every failure
        self.beta = None
        self.tau = None
        self.curr_q_done = 0
        self.cur_failures = 0
        self.max_q_reached = False

        self.name = "GnC (G=" + self.M_guess.name + ")"
        if self.tau_multiplier != 1.0:
            self.name = self.name + "Tm" + str(self.tau_multiplier)
        if not self.use_mgf_width:
            self.name = "Chern" + self.name

    def reset(self):
        # Mechanism.reset()
        self.check_data = None
        self.check_n = None
        self.gamma_list = None  # To store the discretization parameter gamma used at every failure
        self.curr_q_done = 0
        self.cur_failures = 0
        self.max_q_reached = False
        self.M_guess.reset()


    def add_data(self, data_dict):
        """
        Reserves self.check_data_frac fraction of the data as the holdout set, and adds the remaining data to
        the guess mechanism.
        :param data_dict: should be a dict containing a key "data" with value being a matrix, where each row in
                          the matrix represents a sample of the population. Can also contain a key "name" which
                          contains the name of the generated dataset (the same dataset will be generated for the same
                          name in the future)
        """

        self.check_n = int(np.floor(len(data_dict["data"]) * self.check_data_frac))
        self.check_data = data_dict["data"][: self.check_n]
        self.M_guess.add_data({"data": data_dict["data"][self.check_n:]})

    def add_params(self, beta, tau):
        """
        Adds accuracy parameters (tau, beta) to the mechanism.
        :param beta: desired significance (coverage should be at least 1-beta)
        :param tau: width of the certified confidence interval
        """
        self.beta = beta
        self.tau = self.start_tau if self.start_tau > 0.0 else min(self.max_tau, tau)

    @staticmethod
    def myround(ans, gamma, **kwargs):
        """
        Rounds the given value ans to mutiples of gamma  (and within range [0,1])
        :param ans: the value to be rounded
        :param gamma: discretization parameter. ans will be rounded to multiples of gamma (and within range [0,1])
        :returns:
        """
        if ans < 0.0:
            return 0.0
        # Round up, or round down, if specified. Else use default rounding function.
        if "round_down" in kwargs and kwargs["round_down"]:
            mult = int(np.floor(ans / gamma))
        elif "round_up" in kwargs and kwargs["round_up"]:
            mult = int(np.ceil(ans / gamma))
        else:
            mult = round(ans / gamma)
        return min(1.0, mult * gamma)

    @staticmethod
    def encoding_multiplier(x, param=-2):
        """
        Computes the indexed term in a series with sum converging to 1.
        :param x: index of required term in the series
        :param param: if param < 0, use convergent inverse series of power param, else use inverse exponential series
                      of base param
        :returns: i'th element of a series with sum converging to 1
        """

        if x < 0:
            return 1
        elif param < 0:
            return pow(x + 1, param) / sc.special.zeta(-1 * param)
        else:
            return 1.0 / pow(param, x + 1)

    @staticmethod
    def count_possible_transcripts(q, f, discr_f_list=None):
        """
        Computes the number of possible transcripts, given the number of queries asked i, number of failures f, and the
        list of discretization parameter used in each failure discr_f_list.
        :param q: number of queries already asked
        :param f: number of failures occurred
        :param discr_f_list: list containing the discretization parameter used in each failure
        :returns: the total number of possible transcripts with the given parameters
        """

        c = 1.0
        if f > 0:
            if q > f:
                c *= sc.special.comb(q, f)
            if discr_f_list:
                assert len(discr_f_list) == f, (
                            "Length of discretization parameters list discr_f_list should be exactly " +
                            "equal to number of failures f.")
                for disc in discr_f_list:
                    c *= np.ceil(1.0 / disc)
        return c

    def max_valid_gamma(self, beta, tau, n):
        """
        Computes the maximum possible discretization parameter possible while guaranteeing the required accuracy.
        :param beta: desired significance for this query
        :param tau: width of the certified confidence interval
        :param n: size of dataset
        :returns: parameter in [0, tau)
        """

        def lhs(gam):
            return 2 * n * pow(tau - gam, 2)

        rhs = np.log(2.0 / beta)

        if lhs(0.0) <= rhs:
            return 0.0

        gam_high = tau

        # Determine how low does gam_low need to be to satisfy the bound. Will determine precision for the binary
        # search performed next as well.
        gam_low = tau / 10
        while lhs(gam_low) < rhs:
            gam_low /= 10

        tol = gam_low  # set precision to gam_low

        # Use binary search (with tolerance of separation between the range set to tol) to find the highest value of
        # gamma (in multiples of tol) in [gam_low, gam_high] such that lhs(cur_gam) <= rhs
        while gam_high > gam_low + tol:
            cur_gam = gam_low + ((gam_high - gam_low) / 2)
            if lhs(cur_gam) < rhs:
                gam_high = cur_gam
            else:
                gam_low = cur_gam
        return self.myround(gam_low, tol, round_down=True)

    def get_answer(self, query):
        """
        Computes the answer for the given query.
        :param query: function that takes in a dataset, and returns a list of value(s)
        :returns: a list of dictionaries, each with a key "answer" containing the answer(s) on the dataset via GnC,
                  "tau" containing the certified width, and "failures" containing the number of failures that have
                  occurred so far. If a query cannot be answered at the required accuracy, from then on "answer" is
                  None.
        """

        assert self.check_n is not None, "No data added. Add data by calling the function add_data."
        assert self.beta is not None, "No params added. Add data by calling the function add_params."
        if self.max_q_reached:
            return [{"answer": None}]

        ans_list = []
        guess_ans = self.M_guess.get_answer(query)
        check_ans = query(self.check_data)
        assert len(guess_ans) == len(check_ans), "Guess and Check answers should have same length."
        for i in range(len(ans_list), len(guess_ans)):
            curr_q_enc_mult = self.encoding_multiplier(self.curr_q_done, self.q_encoding_param)
            failures_enc_mult = self.encoding_multiplier(self.cur_failures, self.f_encoding_param)
            poss_transcript_div = self.count_possible_transcripts(self.curr_q_done, self.cur_failures, self.gamma_list)
            beta_i = self.beta * curr_q_enc_mult * failures_enc_mult / poss_transcript_div  # accounting for all
            # possible transcripts for previous queries
            width_to_use = self.tau

            if self.use_mgf_width:
                # Get appropriate (right/left) end of the confidence interval depending on which of guess answer
                # and check answer is greater
                if guess_ans[i]["answer"] > check_ans[i]:
                    cur_width = cw.mgf_width(self.check_n, guess_ans[i]["answer"] + width_to_use, beta_i / 2,
                                             upper=True)
                else:
                    cur_width = cw.mgf_width(self.check_n, guess_ans[i]["answer"] - width_to_use, beta_i / 2,
                                             upper=False)
            else:
                cur_width = cw.chernoff_conf(self.check_n, 1, beta_i)

            diff = np.abs(guess_ans[i]["answer"] - check_ans[i])
            self.curr_q_done += 1
            # print("TOLERANCE FOR QUERY # {} IS {}".format(self.curr_q_done, cur_width))
            # cur_width = 0.0
            # diff, cur_width = 0.0, 0.0
            if diff <= width_to_use - cur_width:
                ans_list.append({"answer": guess_ans[i]["answer"], "tau": width_to_use, "failures": self.cur_failures})
            else:
                self.cur_failures += 1
                ans_given = False

                # Compute new tau if self.tau_multiplier != 1.
                if self.tau_multiplier != 1.0:
                    gamma_found = False
                    while not gamma_found:  # increase tau until a non-zero discretization parameter gamma is found
                        self.tau = min(self.tau * self.tau_multiplier, self.max_tau)
                        width_to_use = self.tau
                        gamma_test = self.max_valid_gamma(beta_i, width_to_use, self.check_n)
                        if gamma_test > 0.0:
                            gamma_found = True
                        elif self.tau == self.max_tau:
                            break

                gamma = self.max_valid_gamma(beta_i, width_to_use, self.check_n)

                if gamma > 0.0:
                    ans_list.append({"answer": self.myround(check_ans[i], gamma), "tau": width_to_use,
                                     "failures": self.cur_failures})  # if a valid gamma is found, add discretized
                    # holdout answer to answer list
                    if self.cur_failures > 1:
                        self.gamma_list.append(gamma)
                    else:
                        self.gamma_list = [gamma]
                    ans_given = True

                if not ans_given:
                    break

        if len(ans_list) < len(guess_ans):
            ans_list.append({"answer": None})
            self.max_q_reached = True
        return ans_list


def discretize_get_q(gamma, n, beta, tau):
    """
    Computes the maximum number of queries that can be answered via discretization with parameter gamma,
    given n, beta, tau.
    :param gamma: discretization parameter gamma (to get discretization with multiples of gamma)
    :param n: dataset size
    :param beta: failure probability
    :param tau: tolerance width (radius)
    :returns: the maximum queries that can be answered via discretization with parameter gamma, given n, beta, tau
    """
    save_path = hf.make_dirs(dir_path_list=["Saved_Results"])
    filename = os.path.join(save_path, "Discretize_bounds.pickle")
    to_save_dict = {"n": n, "beta": beta, "tau": tau, "gamma": gamma}
    to_save_key = tuple(OrderedDict(sorted(to_save_dict.items())).items())
    bound = hf.get_from_file(filename, to_save_key, log=False)  # check if already computed
    if bound is None:
        # Binary search in range [1, n] to find max. number of queries that can be answered
        # combining compression bounds with the Chernoff bound, for computing accuracy of discretized answer
        # For answering q queries, we require 2 * (1/gamma)^(q-1) exp(-2 * (tau-gamma)^2 * n) < beta/q
        q_high = n
        q_low = 1
        lhs = np.log(beta)
        while q_high > q_low + 1:
            q_mid = int(np.ceil(q_high + q_low) / 2)
            rhs = np.log(2 * q_mid) - 2 * n * pow(tau - gamma, 2) + (q_mid - 1) * np.log(1.0 / gamma)
            if lhs >= rhs:
                q_low = q_mid
            else:
                q_high = q_mid - 1
        bound = q_low
        hf.write_to_file(filename, to_save_key, bound, log=False)
    return bound


def discretize_get_best_q(gamma_list, n, beta, tau):
    """
    Computes the maximum number of queries that can be answered via discretization with gamma from gamma_list,
    given n, beta, tau.
    :param gamma_list: list containing discretization parameter gamma (to get discretization with multiples of gamma)
    :param n: dataset size
    :param beta: failure probability
    :param tau: tolerance width (radius)
    :returns: the maximum queries that can be answered via discretization from gamma_list, given n, beta, tau
    """
    max_q = 0
    for gamma in gamma_list:
        if gamma <= tau:  # If gamma > tau, not possible to guarantee accuracy with tolerance tau.
            q_cur = discretize_get_q(gamma, n, beta, tau)
            if q_cur > max_q:
                max_q = q_cur
    return max_q
