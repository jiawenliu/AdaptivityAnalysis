# -*- coding: utf-8 -*-

import math
import numpy as np
import scipy.stats
import scipy.optimize


def mgf_width(n, p, beta, upper=True):
    """
    Computes the range of the observed mean that would lead to a confidence interval (CI) whose left
    (respectively, right) end point is above (resp., below) p.
    :param n: number of observations
    :param p: desired left/right end of certified interval
    :param beta: desired significance (coverage should be at least 1-beta)
    :param upper: True for right end of certified interval, False for left end.
    :returns: left or right width (distance between max/min feasible observed mean and end point p)
    """
    target_log_prob = np.log(beta)

    if upper:
        p = 1 - p  # As Pr(B < p-w) = Pr(B' > p' + w), where p' = 1-p

    def mgf_bound_log(w):
        def f(lamb):
            # For every lamb >= 0, this gives an upper bound on the log of Pr(S > n(p+w))
            # if S is a sum of independent [0,1] r.v.'s with mean at most p.
            # Bound is obtained by applying Markov to the MGF (and taking logs)

            ans = n * (np.log(1 + p * (np.exp(lamb) - 1)) - lamb * (p + w))  # equals n* (ln(MGF(lamb)) - lamb * t)
            # print("n: " + str(n) + " p: " + str(p) + " w: " + str(w) + " lamb: " + str(lamb) + " ans: " + str(ans))
            return ans

        # Try to find the tightest upper bound by searching over lamb between 0 and 10.

        res = scipy.optimize.minimize(f, p, bounds=((0, None),), tol=10 ** (-7))
        return f(res.x)

    # Now look for the right width (between 0 and 1-p)
    w_high = 1.0 - p
    w_low = 0.0

    tol = 10 ** (-7)

    # Use binary search (with tolerance of separation between the range set to tol) to find the lowest value of
    # w in [w_low, w_high] such that lhs(cur_w) <= rhs
    while w_high > w_low + tol:
        cur_w = (w_high + w_low) / 2
        l_cur_w = mgf_bound_log(cur_w)
        # mgf_bound_log is a descending function
        # if the function value at mid point is too high, recurse on the right half
        if l_cur_w > target_log_prob:
            w_low = cur_w
        else:  # else recurse on left half.
            w_high = cur_w

    return w_high


def chernoff_conf(n, q, beta):
    """
    Use Chernoff Bound: Pr[|f(Dist) - f(sample)| > \tau] <= 2 exp(-2 \tau^2 n)
    :param n: total number of samples
    :param q: total number of queries
    :param beta: desired significance (coverage should be at least 1-beta)
    :returns: the confidence width that is guaranteed by each answer given using fresh n/q samples from the dataset
    """

    n_split = int(np.floor(n / q))
    x = int(np.floor(n_split/2))
    while scipy.stats.binom.cdf(x, n_split, .5) < 1 - beta / (2 * q):
        x += 1
    width = float(np.abs(x - float(n_split) / 2)) / n_split
    return width


def adv_comp(q, eps, delta, method="DP"):
    """
    Get epsilon value from the application of advanced composition to q (eps, delta)-DP mechanisms
    :param q: number of mechanisms
    :param eps: privacy parameter in (eps,delta)-DP for each of the q mechanisms
    :param delta: privacy parameter in (eps,delta)-DP for each of the q mechanisms
    :param method: method for calculating composition bound. Currently, should be from {"DP", "CDP"}
    :returns: privacy cost (computed via advanced composition) of q (eps,delta)-DP mechanisms.
    """

    if method == "DP":
        return (math.exp(eps) - 1) / (math.exp(eps) + 1) * eps * q / 2 + eps * math.sqrt(2 * q * math.log(1 / delta))
    if method == "CDP":
        rho = eps
        if math.sqrt(q * math.pi * rho) / delta < math.exp(1):
            val = 1 / delta
        else:
            val = math.sqrt(q * math.pi * rho) / delta
        if val < 1.0:
            val = 1.0
        return rho * q + 2 * math.sqrt(rho * q * math.log(val))


def conf_width_BNSSSU(n, q, beta):
    """
    Uses a better version of the main transfer theorem (with improved constants) but uses non-optimal
    composition - applies for alpha, beta < 1. Currently applicable only for Gaussian noise.
    :param n: total number of samples
    :param q: total number of queries
    :param beta: desired significance (coverage should be at least 1-beta)
    :returns: the confidence width that is guaranteed via BNSSSU'16 for each answer
    """

    def g1(p):  # p = [rho, delta]
        epsilon = adv_comp(q, p[0], p[1], method="CDP")
        lhs = (math.exp(epsilon) - 1 + 2 * math.floor(float(1) / beta) * p[1])
        rhs = float(1) / n * math.sqrt(float(1) / p[0] * math.log(max(float(4) * q / p[1], 1.0)))
        return lhs + rhs

    def guess_for_BNSSSU(n, q, beta, x0):
        t = math.floor(1 / beta)

        def g(x):  # x = [rho,beta']
            return math.sqrt(t * x[0] * q / 2) + 1 / n * math.sqrt(1 / x[0] * (math.log(2 * q / x[1]))) + 2 * t * x[1]

        bounds = ((10 ** (-15), 1.0 / math.sqrt(q)), (10 ** (-15), 1))
        res = scipy.optimize.minimize(g, x0, bounds=bounds, tol=10 ** (-15))
        return res.x

    lower = 10 ** (-15)
    upper = 1.0 / math.sqrt(q)
    bounds = ((lower, upper), (lower, upper))
    res = scipy.optimize.minimize(g1, guess_for_BNSSSU(n, q, beta, [.001, .000001]),
                                  bounds=bounds, tol=10 ** (-15))
    alpha = min(1, g1(res.x) / (1 - float(1 - beta) ** (math.floor(1.0 / beta))))
    return alpha


def conf_width_DFHPRR_new(n, q, beta):
    """
    Based off of Theorem 10 in DFHPPR'16. Currently applicable only for Gaussian noise.
    :param n: total number of samples
    :param q: total number of queries
    :param beta: desired significance (coverage should be at least 1-beta)
    :returns: the confidence width that is guaranteed via DFHPRR'16 for each answer
    """

    def h(rho):
        roots = np.roots([1.0, -8.0 * rho * q,
                          16.0 * math.pow(rho * q, 2) - 64 * rho * q * math.log(math.sqrt(math.pi * rho * q)),
                          -64.0 * rho * q * math.log(1.0/beta)])  # eqn corresponding to 2nd constraint in Theorem B.2
        max_root = np.real(np.max(roots))
        inn = max_root + math.sqrt(math.log(4.0 * q / beta) / rho) / (2.0 * n)
        return inn

    res = scipy.optimize.minimize_scalar(h, bounds=(0.0, 10 ** (-7)), method='bounded', tol=10 ** (-10))
    at_least = 2.0 * math.sqrt(48 / n * math.log(8 / beta))  # eqn corresponding to 1st constraint in Theorem B.2
    ans = np.min([1, np.max([at_least, h(res.x)])])
    return ans

    
def conf_width_RZ_new(n, q, beta, sigma=-1.0):
    """
        Based off of Theorem 2.1 (combining CDP (BS'16) + BNSSSU'16 + RZ'16) in the paper. Currently applicable only for
        Gaussian noise.
        :param n: total number of samples
        :param q: total number of queries
        :param beta: desired significance (coverage should be at least 1-beta)
        :param sigma: if sigma is -1, calculates width for best value of sigma. Otherwise, calculates width for given
            sigma
        :returns: the confidence width that is guaranteed via CDP (BS'16) + BNSSSU'16 + RZ'16 for each answer
    """

    def g3(x):  # x = rho*q*n
        def f(y):  # y = lambda
            return float(2 * x - np.log(1 - y)) / y

        res_inn = scipy.optimize.minimize_scalar(f, bounds=(0.0, 1.0 - 10 ** (-15)), method='bounded')
        term1 = (float(1) * res_inn.fun) / (2 * n * beta)
        rho0 = x / (q * n)
        if term1 >= 0:
            return math.sqrt(term1) + 1.0 / (2 * n) * math.sqrt(math.log(4 * q / beta) / rho0)
        else:
            return 10 ** 16

    if sigma == -1:
        res0 = scipy.optimize.minimize_scalar(g3, bounds=(0.0, 10 ** 16), method='bounded')
        return res0.fun
    else:
        rho1 = 1.0 / (2 * pow(n * sigma, 2))
        return g3(rho1 * q * n)


def conf_width_XR_new(n, q, beta):
    """
    Based off a result derived similar to the proof of Theorem 2.1 (combining CDP (BS'16) + BNSSSU'16 + XR'17) in the
    paper. Currently applicable only for Gaussian noise.
    :param n: total number of samples
    :param q: total number of queries
    :param beta: desired significance (coverage should be at least 1-beta)
    :returns: the confidence width that is guaranteed via CDP (BS'16) + BNSSSU'16 + XR'17 for each answer
    """

    def g3(x):  # x = q n \rho'
        term1 = (float(2) / n) * ((2 * x / beta) + math.log(4 / beta, 2))
        rho0 = x / (q * n)
        if term1 >= 0:
            return math.sqrt(term1) + 1.0 / (2 * n) * math.sqrt(math.log(4 * q / beta) / rho0)
        else:
            return 10 ** 16

    res0 = scipy.optimize.minimize_scalar(g3, bounds=(0.0, 10 ** 16), method='bounded')
    return min(1, res0.fun)


def bounds_Thresh(h, thresh, q, b, sigma, beta=0.0, only_last_query=False):
    """
    Based off of Theorem B.9 in the paper.
    :param h: number of samples in the holdout set
    :param thresh: threshold used for comparing training and holdout answers
    :param q: total number of queries asked
    :param b: number of queries answered via the holdout set
    :param sigma: noise parameter for Laplace noise added to the comparisons
    :param beta: desired significance (coverage should be at least 1-beta)
    :param only_last_query: whether coverage required only for the last query asked
    :returns: the confidence width that is guaranteed for Thresholdout
    """
    def f(y):  # y = lambda
        return float((2 * b / (h * pow(sigma, 2))) - np.log(1 - y)) / y

    res_inn = scipy.optimize.minimize_scalar(f, bounds=(0.0, 1.0 - 10 ** (-11)), method='bounded')
    comps = [res_inn.fun / (4 * h)]
    if only_last_query:
        laps = 0.0
        laps_sq = ((2 * pow(4 * sigma, 2)) + (2 * pow(2 * sigma, 2))
                   + 2 * math.sqrt(2 * pow(4 * sigma, 2) * 2 * pow(2 * sigma, 2)))
    else:
        laps_sum = 0.0
        laps_sq_sum = 0.0
        exp_over = 100  # Calculate avg over exp_over samples of sum of max of q Laplace r.v.'s and b Laplace r.v.'s
        for _ in range(exp_over):
            w_max = max(np.random.laplace(scale=4*sigma, size=q))
            y_max = max(np.random.laplace(scale=2*sigma, size=b)) if b > 0 else 0
            laps_sum += (w_max + y_max)
            laps_sq_sum = pow((w_max + y_max), 2)
        laps = laps_sum / exp_over
        laps_sq = laps_sq_sum / exp_over

    psi = laps_sq + 2 * thresh * laps
    comps.append(pow(thresh, 2) + psi)

    temp = res_inn.fun * comps[1] / h
    if temp >= 0:
        comps.append(math.sqrt(temp))
    else:
        comps.append(10 ** 16)
    if beta == 0.0:
        return sum(comps)
    else:
        return math.sqrt(sum(comps)/beta)
