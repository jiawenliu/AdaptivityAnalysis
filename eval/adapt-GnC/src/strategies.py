# -*- coding: utf-8 -*-

import helper_funcs as hf
import numpy as np
import math

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
        assert ada_freq["method"] in {"additive", "power", "repeated_query_subroutine", "lil_ucb", "lrgd", "n_dim_pairwise", "c_adaptivity", "n_adaptivity", "multiple_rounds", "mr_odd", "mr_single"}, ("Adaptive query frequency method should be in " +
                                                             "{additive, power, repeated_query_subroutine, n_dim_pairwise, lrgd, mr_odd, mr_single, multiple_rounds}")

        self.ada_method = ada_freq["method"]
        self.ada_method_param = ada_freq["method_param"]
        self.name = "Adaptive_every_" + self.ada_method + "_" + str(self.ada_method_param)
        if self.ada_method == "additive":
            assert ada_freq["method_param"] > 1, "Adaptive query frequency should be greater than 1."
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

        self.q_max += 1
        self.cardinality += 1

        
        if data_name is not None:
            self.data_name = data_name
            hf.initialize_with_str_seed(data_name)
        data = np.random.choice([-1, 1], (self.n, self.cardinality), p=[1 - self.pr_1, self.pr_1])

        return data

    def gen_data_decimal(self, data_name=None):
        """
        Generates data for the strategy.
        :param data_name: name for the generated data
        :returns: an (self.n x self.cardinality) array, with each entry in (0, 1)
        """
        self.q_max += 1
        self.cardinality += 1
        
        if data_name is not None:
            self.data_name = data_name
            hf.initialize_with_str_seed(data_name)
        data = np.random.rand(self.n, self.cardinality)

        return data

    def gen_data_arm_samples(self, arms, data_name=None):
        """
        Generates data for the strategy.
        :param data_name: name for the generated data
        :returns: an (self.n x self.cardinality) array, with each entry in (0, 1)
        """
        self.q_max += 1
        self.cardinality += 1
        
        if data_name is not None:
            self.data_name = data_name
            hf.initialize_with_str_seed(data_name)
        data = np.array([np.random.normal(arm, 1.0, self.n) for arm in arms]).transpose()

        return data


    def gen_data_integer(self, data_name=None):
        """
        Generates data for the strategy.
        :param data_name: name for the generated data
        :returns: an (self.n x 1) array, with each entry in range [self.cardinality]
        """

        # If data_name is not None, generate data after initializing random number generator with a seed corresponding
        # to data_name, so that the same data is generated each time for a given data_name (for consistency across
        # multiple runs of the strategy).
        self.q_max += 1
        self.cardinality += 1

        
        if data_name is not None:
            self.data_name = data_name
            hf.initialize_with_str_seed(data_name)
        data = np.random.choice(range(self.cardinality), (self.n))

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

            ret_ans = np.count_nonzero(pred_x == data[:, -1]) * 1.0 / s  # calculate accuracy of predictions

            if self.q_mean != 0.5:
                ret_ans *= self.q_mean/max(self.pr_1, 1 - self.pr_1)  # scaling answer if self.q_mean != 0.5
            return [ret_ans]


        def get_repeated_query_subroutine(data):
            data_size, dimension = data.shape
            x = np.random.choice(data_size)
            y = np.random.choice(data_size)

            compare = ((np.sum(data[x, :]) 
                       - np.sum(data[y, :])) / dimension) / 2.0   # each answer in [-1.0, 1.0]

            return [(compare + 1) / 2.0]  # each answer in [0.0, 1.0]

        def n_dim_pairwise_query(data):
            data_size, dimension = data.shape
            x = np.random.choice(data_size)
            y = np.random.choice(data_size)

            compare = ((np.sum(data[x, :]) 
                       - np.sum(data[y, :])) / dimension) / 2.0   # each answer in [-1.0, 1.0]

            return [(compare + 1) / 2.0]  # each answer in [0.0, 1.0]
        
        def lil_ucb_query(gate, para):
            def lil_ubc_query_sub(data):
                ans = 1.0 / gate * sum(sum(data[:gate])) + (1.0 + para.beta) * (1.0 + math.sqrt(para.epsilon)) * math.sqrt(2.0 * (para.sigma**2) * (1.0 + para.epsilon) * math.log(math.log((1.0 + para.epsilon) * gate)/para.confidential_interval) / gate)
                return [ans] 

            return  lil_ubc_query_sub # each answer in [0.0, 1.0]

        def best_arm(para):
            def best_arm_sub(data):
                i = para.curr_arm
                gate = para.gate[i] 
                ans = (1.0 / gate * sum(data[1:(gate+1), i]) + (1.0 + para.beta) * (1.0 + math.sqrt(para.epsilon)) * math.sqrt(2.0 * (para.sigma**2) * (1.0 + para.epsilon) * math.log(math.log((1.0 + para.epsilon) * gate)/para.confidential_interval) / gate))
                return [ans] 
            

            return  best_arm_sub # each answer in [0.0, 1.0]


        def c_adaptivity_query(data):
            data_size, dimension = data.shape
            x = np.random.choice(data_size)
            y = np.random.choice(data_size)

            compare = ((np.sum(data[x, :]) 
                       - np.sum(data[y, :])) / dimension) / 2.0   # each answer in [-1.0, 1.0]

            return [(compare + 1) / 2.0]  # each answer in [0.0, 1.0]

        def lrgd(para):
            def lrgd_sub(data):
                data_size = len(data)
                if para.degree == 0:
                    ans = (-2) *(np.mean([data[i, 1] - (data[i,0] * para.coefficient[1] + para.coefficient[0]) for i in range(data_size)]))
                elif para.degree == 1:
                    ans = (-2) * np.mean([ (data[i, 1] - (data[i,0] * para.coefficient[1] + para.coefficient[0])) * data[i,0] for i in range(data_size)])
                else:
                    ans = None
                return [ans]
            return lrgd_sub

        def mr(para):
            def mr_sub(data):
                data_size = len(data)
                ans = []
                for d in data:
                    if d not in para.traced:
                        ans.append(np.random.choice([0, 1.0], p =  [1 - self.pr_1, self.pr_1]))
                return [np.sum(ans) / data_size]
            return mr_sub

        if self.ada_method == "c_adaptivity":
            if self.cur_q >= self.q_max:
                if prev_ans:
                    self.mech_ans_list.append(prev_ans[0]["answer"])
                return None
            # if prev_ans[0]["true_answer"]:
            #      self.true_ans_list.append(prev_ans[0]["true_answer"]) 
            true_ans = np.random.choice([-1, 1], p=[1 - self.pr_1, self.pr_1])
            self.cur_q += 1
            self.true_ans_list.append(true_ans) 
            if prev_ans:
                self.mech_ans_list.append(prev_ans[0]["answer"])
            return {"query": c_adaptivity_query, "true_answer": true_ans}

        if self.ada_method == "n_adaptivity":
            if self.cur_q >= self.q_max:
                if prev_ans:
                    self.mech_ans_list.append(prev_ans[0]["answer"])
                return None
            # if prev_ans[0]["true_answer"]:
            #      self.true_ans_list.append(prev_ans[0]["true_answer"]) 
            true_ans = np.random.choice([-1, 1], p=[1 - self.pr_1, self.pr_1])
            self.cur_q += 1
            self.true_ans_list.append(true_ans) 
            if prev_ans:
                self.mech_ans_list.append(prev_ans[0]["answer"])
            return {"query": c_adaptivity_query, "true_answer": true_ans}

        if self.ada_method == "multiple_rounds":
            if self.cur_q >= self.q_max:
                if prev_ans and "answer"  in prev_ans[0].keys():
                    self.mech_ans_list.append(prev_ans[0]["answer"])
                return None
            if prev_ans:
                if "answer"  in prev_ans[0].keys():
                    self.mech_ans_list.append(prev_ans[0]["answer"])
                para = prev_ans[0]["para"]
            else:
                para = None

            true_ans = self.q_mean
            self.cur_q += 1
            self.true_ans_list.append(true_ans) 

            return {"query": mr(para), "true_answer": true_ans}


        if self.ada_method == "lrgd":
            if self.cur_q >= self.q_max:
                if prev_ans and "answer"  in prev_ans[0].keys():
                    self.mech_ans_list.append(prev_ans[0]["answer"])
                return None
            if prev_ans:
                if "answer"  in prev_ans[0].keys():
                    self.mech_ans_list.append(prev_ans[0]["answer"])
                para = prev_ans[0]["para"]
            else:
                para = None

            true_ans = self.q_mean
            self.cur_q += 1
            self.true_ans_list.append(true_ans) 

            return {"query": lrgd(para), "true_answer": true_ans}

        if self.ada_method == "mr_odd":
            if self.cur_q >= self.q_max:
                if prev_ans and "answer"  in prev_ans[0].keys():
                    self.mech_ans_list.append(prev_ans[0]["answer"])
                return None
            if prev_ans:
                if "answer"  in prev_ans[0].keys():
                    self.mech_ans_list.append(prev_ans[0]["answer"])
                para = prev_ans[0]["para"]
            else:
                para = None

            true_ans = self.q_mean
            self.cur_q += 1
            self.true_ans_list.append(true_ans) 

            return {"query": lrgd(para), "true_answer": true_ans}

        if self.ada_method == "mr_single":
            if self.cur_q >= self.q_max:
                if prev_ans and "answer"  in prev_ans[0].keys():
                    self.mech_ans_list.append(prev_ans[0]["answer"])
                return None
            if prev_ans:
                if "answer"  in prev_ans[0].keys():
                    self.mech_ans_list.append(prev_ans[0]["answer"])
                para = prev_ans[0]["para"]
            else:
                para = None

            true_ans = self.q_mean
            self.cur_q += 1
            self.true_ans_list.append(true_ans) 

            return {"query": lrgd(para), "true_answer": true_ans}

        if self.ada_method == "best_arm":
            if self.cur_q >= self.q_max:
                if prev_ans and "answer"  in prev_ans[0].keys():
                    self.mech_ans_list.append(prev_ans[0]["answer"])
                return None
            if prev_ans:
                if "answer"  in prev_ans[0].keys():
                    self.mech_ans_list.append(prev_ans[0]["answer"])
                para = prev_ans[0]["para"]
            else:
                para = None

            true_ans = para.arms[para.curr_arm]
            self.cur_q += 1
            self.true_ans_list.append(true_ans) 

            return {"query": best_arm(para), "true_answer": true_ans}


        if self.ada_method == "lil_ucb":
            if self.cur_q >= self.q_max:
                if prev_ans:
                    self.mech_ans_list.append(prev_ans[0]["answer"])
                return None
            if prev_ans:
                if "answer"  in prev_ans[0].keys():
                    self.mech_ans_list.append(prev_ans[0]["answer"])
                gate = prev_ans[0]["gate"]
                para = prev_ans[0]["para"]
            else:
                gate = 0 
                para = None

            true_ans = self.q_mean * gate
            self.cur_q += 1
            self.true_ans_list.append(true_ans) 

            return {"query": lil_ucb_query(gate, para), "true_answer": true_ans}



        if self.ada_method == "n_dim_pairwise":
            if self.cur_q >= self.q_max:
                if prev_ans:
                    accumulated_answer = self.mech_ans_list[-1] * len(self.mech_ans_list) + prev_ans[0]["answer"]
                    self.mech_ans_list.append(accumulated_answer/(self.cur_q))
                return None
            true_ans = self.q_mean
            self.cur_q += 1
            self.true_ans_list.append(true_ans) 
            if prev_ans:
                accumulated_answer = prev_ans[0]["answer"] if self.cur_q <= 2 else self.mech_ans_list[-1] * len(self.mech_ans_list) + prev_ans[0]["answer"]
                self.mech_ans_list.append(accumulated_answer / self.cur_q)

            return {"query": n_dim_pairwise_query, "true_answer": true_ans}

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


