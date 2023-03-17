# -*- coding: utf-8 -*-

import helper_funcs as hf
import numpy as np


class Strategy:
    """
    Base class which defines the structure for analyst strategies. Compatible with mechanisms having fixed
    dataset size n, desired significance beta (coverage should be at least 1-beta), and tolerance width tau.
    """

    def __init__(self, n, q_mean = 0.5, ada_freq = None, q_max = None, cardinality = None):
        """
        Initializer for the strategy.
        :param n: dataset size
        :param q_mean: expectation of the answer for each query asked by the strategy
        :param ada_freq: denotes adaptive query frequency. If None, every 100th query will be asked adaptively.
                         Otherwise, ada_freq should be a dictionary having two keys, method, and method_param > 1.
                         method="additive" denotes adaptive queries will be asked at indices that are multiples of
                         method_param , whereas method="power" denotes adaptive queries will be asked at indices i for
                         which there exists j, 0<j<i, such that pow(j, method_param) = i.
        :param q_max: maximum number of queries q_max asked by the strategy. If None, q_max is assigned the maximum
                      value q such that q <= n and the last query is an adaptive query for the strategy.
        """
        assert 0 <= q_mean <= 1.0, "Expectation of each query, q_mean, should be in [0.0, 1.0]."
        self.n = n
        self.q_mean = q_mean
        if ada_freq is None:
            ada_freq = {"method": "additive", "method_param": 100}

        self.pr_1 = (1.0 + np.sqrt(max(2 * q_mean - 1, 1 - 2 * q_mean))) / 2  # Calculating the probability of 1, pr_1,
        # so that expectation of each non-adaptive query = q_mean. For each adaptive query, we scale it such that its
        # expectation is q_mean. Each non-adaptive query q_i = ((x(i) * x(q_max)) + 1) / 2, where x \in {-1,1}^(q_max).

        self.ada_ctr = 0  # adaptive query counter

        assert "method" in ada_freq and "method_param" in ada_freq, ("Adaptive query frequency should have method"
                                                                     + " type and method parameter.")
        assert ada_freq["method"] in {"additive", "power", "repeated_query_subroutine"}, ("Adaptive query frequency method should be in " +
                                                             "{additive, power, repeated_query_subroutine}")
        assert ada_freq["method_param"] > 1, "Adaptive query frequency should be greater than 1."

        self.ada_method = ada_freq["method"]
        self.ada_method_param = ada_freq["method_param"]
        self.name = "Adaptive_every_" + self.ada_method + "_" + str(self.ada_method_param)
        if self.ada_method == "additive":
            if q_max is None:
                q_max = int(np.floor(self.n/self.ada_method_param)) * self.ada_method_param
            else:
                assert q_max % self.ada_method_param == 0, ("q_max should be divisible by additive frequency of adaptive queries.")
            self.next_ada_q_at = self.ada_method_param
        elif self.ada_method == "power":
            if q_max is None:
                n_root = int(np.floor(pow(n, 1.0 / self.ada_method_param)))
                q_max = pow(n_root, self.ada_method_param)
            else:
                q_max_root = int(np.floor(pow(q_max, 1.0 / self.ada_method_param)))
                assert pow(q_max_root, self.ada_method_param) == q_max, ("q_max should be divisible by power " + str(self.ada_method_param) +  " frequency of adaptive queries.")
            self.next_ada_q_at = pow(self.ada_ctr + 2, self.ada_method_param)
        else:
            if q_max is None:
                q_max = self.n - 2


        self.q_max = q_max
        self.cur_q = 0  # stores the number of queries asked by the strategy
        self.prev_ada_q_at = 0  # stores the index for the last adaptive query
        self.cur_corr = 0  # stores the index of the feature for which the next non-adaptive query will measure
        # correlation with the last feature

        self.mech_ans_list = []  # for storing mechanism's answers to adaptive queries
        self.true_ans_list = []  # for storing true answers to adaptive queries
        self.mech_corr_list = []  # for storing mechanism's answers to non-adaptive queries
        self.true_corr_list = []  # for storing true answers to non-adaptive queries

        if self.q_mean != 0.5:
            self.name = self.name + "_qmean_" + str(self.q_mean)
        if self.q_max != 40000:
            self.name = self.name + "_qmax_" + str(self.q_max)
        self.data_name = None


        if cardinality is None:
            self.cardinality = self.q_max
        else:
            self.cardinality = cardinality




    def gen_data(self, data_name=None):
        """
        Generates data for the strategy.
        :param data_name: name for the generated data
        :returns: an (self.n x self.q_max) array, with each entry in {-1, 1}
        """

        # If data_name is not None, generate data after initializing random number generator with a seed corresponding
        # to data_name, so that the same data is generated each time for a given data_name (for consistency across
        # multiple runs of the strategy).

###################################### Debugging Code: ######################################
        # If column number = q_max, and q_max % ada_method_param == 0,
        # Then the maximum query number we will ask in total is non-adaptive queries + adaptive queries.
        # non-adaptive queries will be 
        # batchs * ada_method_param = (column number / ada_method_param) * ada_method_param = q_max / ada_method_param) * ada_method_param = q_max.
        # So the maximum query we want to ask is indeed more than q_max.
        # In this sense, the last bath of the data base is never used, and we cannot instantiate the set up with only 1 bath.

        # self.q_max += self.q_max/self.ada_method_param
        self.q_max += 1
        self.cardinality += 1

###################################### Debugging Code ^^ ######################################
        
        if data_name is not None:
            self.data_name = data_name
            hf.initialize_with_str_seed(data_name)
        data = np.random.choice([-1, 1], (self.n, self.cardinality), p=[1 - self.pr_1, self.pr_1])

        return data

    def next_query(self, prev_ans=None):
        """
        Computes the next query for the strategy.
        :param prev_ans: previous answer given by the mechanism. Can take value None either when the strategy has asked
                         0 queries, or when the mechanism cannot answer the previous query at the desired accuracy
        :returns: dictionary containing keys "query" and "true_answer". "query" contains a function denoting the next
                  adaptive query, or the next batch of non-adaptive queries (non-adaptive queries until the next
                  adaptive query). "true_answer" contains the expected answer(s) for the query/queries in "query". If
                  number of allowable queries is exhausted, or the mechanism responds None to previously asked query,
                  then returns None.
        """

        # def get_

        def get_corr(data):
            """
            Computes the correlation for each of the next batch of features, with the last feature. Here, batch refers
            to the number of non-adaptive queries before the next adaptive query.
            :param data: 2-d dataset with self.q_max columns
            :returns: list containing answers in [0,1], one each for the next batch of features
            """
            s, k = data.shape
            assert k == self.q_max, "Length of each sample should be " + str(self.q_max)

            corr = (np.sum(np.multiply(
                data[:, self.cur_corr:self.cur_corr + (self.next_ada_q_at - self.prev_ada_q_at - 1)],
                np.reshape(data[:, -1], (s, 1))), axis=0) / s)   # each answer in [-1.0, 1.0]

###################################### Original Code: (is wrong) ######################################

            # return (corr + 1) / 2  # each answer in [0.0, 1.0]

###################################### Original Code ^^ ######################################

###################################### Debugging Code: ######################################
            # print("Corrs Expected by Each Adaptive Query:", (corr + 1) / 2.0)
            # print("Corrs Computed from Non-Adaptive Queries:", (corr + 1) / 2)

            return (corr + 1) / 2.0  # each answer in [0.0, 1.0]

###################################### Debugging Code ^^ ######################################



        def get_acc(data):
            """
            Computes the adaptive query using the answers of previously asked non-adaptive queries.
            :param data: 2-d dataset with self.q_max columns
            :returns: list containing an answers in [0,1]
            """
            s, k = data.shape
            assert k == self.q_max, "Length of each sample should be " + str(self.q_max)

            corr_arr = (np.array(self.mech_corr_list) * 2) - 1  # each answer in [-1.0, 1.0]
            corr_arr = [max(-1 + 1e-15, min(1 - 1e-15, corr)) for corr in corr_arr]  # to avoid errors in
            # log computation
            weight_arr = [np.log((1.0 + corr) / (1 - corr)) for corr in corr_arr]  # weight of each queried feature in
            # predicting last feature
            pred_x = (np.sign(np.sign(np.dot(data[:, : self.cur_corr], weight_arr)) + 1) * 2) - 1  # predictions in
            # [-1,1]^s
###################################### Original Code: (is wrong) ######################################

            # ret_ans = np.count_nonzero(pred_x == data[:, -1]) / s  # calculate accuracy of predictions

###################################### Original Code ^^ ######################################

###################################### Debugging Code: ######################################

            ret_ans = np.count_nonzero(pred_x == data[:, -1]) * 1.0 / s  # calculate accuracy of predictions

###################################### Debugging Code ^^ ######################################
            if self.q_mean != 0.5:
                ret_ans *= self.q_mean/max(self.pr_1, 1 - self.pr_1)  # scaling answer if self.q_mean != 0.5
            return [ret_ans]

        # def get_lil_ucb_query(data):
        #     return 1.0 / gate * sum(data[:gate]) + (1 + beta) * (1 + math.sqrt(epsilon)) * math.sqrt(2 * (sigma**2) * (1 + epsilon) * math.log(math.log((1 + epsilon) * gate)/confidential_interval) / gate)

        def get_repeated_query_subroutine(data):
            data_size, _ = data.shape
            x = np.random.choice(data_size)
            y = np.random.choice(data_size)

            compare = (np.sum(data[x, :]) 
                       - np.sum(data[y, :]))   # each answer in [-1.0, 1.0]

            return [compare]  # each answer in [0.0, 1.0]

        if self.ada_method == "repeated_query_subroutine":
            if self.cur_q >= self.q_max:
                if prev_ans:
                    self.mech_ans_list.append(prev_ans[0]["answer"])
                return None
            true_ans = (-1) * (1 - self.pr_1) + 1 * self.pr_1
            self.cur_q += 1
            self.true_ans_list.append(true_ans) 
            if prev_ans:
                self.mech_ans_list.append(prev_ans[0]["answer"])
            return {"query": get_repeated_query_subroutine, "true_answer": true_ans}


        if self.cur_q < self.q_max and (self.cur_q == 0 or prev_ans):  # continue as long as number of queries asked
            # doesn't exceed self.q_max, and one of either the mechanism provided an answer to the previous query, or

            # it is the first query
            if prev_ans:  # append mechanism answer to appropriate list
                if self.cur_q == self.prev_ada_q_at:
                    self.mech_ans_list.append(prev_ans[0]["answer"])
                else:
                    for i in range(len(prev_ans)):
                        self.mech_corr_list.append(prev_ans[i]["answer"])

            self.cur_q += 1
            if self.cur_q == self.next_ada_q_at:  # if the next query to be asked is an adaptive query
                self.cur_corr += (self.next_ada_q_at - self.prev_ada_q_at - 1)  # incorporate the number of
                # non-adaptive queries (correlations) asked so far
                self.prev_ada_q_at = self.next_ada_q_at
                self.ada_ctr += 1
                # Compute the index of the next adaptive query, based on the method self.ada_method
                if self.ada_method == "additive":
                    self.next_ada_q_at += self.ada_method_param
                else:
                    self.next_ada_q_at = pow(self.ada_ctr + 2, self.ada_method_param)
                true_ans = self.q_mean  # true answer for the adaptive query
                self.true_ans_list.append(true_ans)  # append to the appropriate list
                return {"query": get_acc, "true_answer": [true_ans]}
            else:
                true_ans = [self.q_mean] * (self.next_ada_q_at - self.prev_ada_q_at - 1)  # compute the true answers
                # for the next batch of non-adaptive queries
                self.true_corr_list.extend(true_ans)  # append to the appropriate list
                self.cur_q += (self.next_ada_q_at - self.prev_ada_q_at - 2)  # increment self.cur_q to incorporate
                # asking the next batch of non-adaptive queries
                return {"query": get_corr, "true_answer": true_ans}



        return None  # if exhausted number of allowable queries, or mechanism responded None for previous query


