import math
import numpy as np
import matplotlib as mpl
# mpl.use('tkagg')
import matplotlib.pyplot as plt
import random

def getScale(k, n):
	return (math.sqrt(math.sqrt(k / math.log(k) )/ n))


def sign(n):
    if n > 0 : 
        return 1.0
    else:
        return (-1.0)

def signM(x, n, k):
  for i in range (n):
     for j in range (k):
       x[i][j] = sign(x[i][j])
  return x


def genData(N, n):
	# return range(n)
	return [random.randint(0, N) for _ in range(n)]
    # return random.sample(range(N), n)


def noNoiseMech(x, query):
  # return random.random() if set(x) == {-1} else (sum([query(xi) for xi in x]) * 1.0 / len(x))
  return (sum([query(xi) for xi in x]) * 1.0 / len(x)) if len(x) > 0 else random.random()

def gaussianMech(x, query, sigma = 0.01):
  return (sum([query(xi) for xi in x]) * 1.0 / len(x) + np.random.normal(loc=0.0, scale=sigma)) if len(x) > 0 else random.random()

def populationMean(N, p):
	return p


def updatePopulationScores(population_scores, control_set_scores, a, query, p, I):
	N, c = len(population_scores), len(control_set_scores)
	if N >= c:
		return [control_set_scores[i] + (a - p) * ( query(i) - p) if i not in I else control_set_scores[i] for i in range(c)] + [population_scores[i] + (a - p) * ( query(i) - p) if i not in I 
				else  population_scores[i] for i in range(c, N)]
	else:
		return [control_set_scores[i] + (a - p) * ( query(i) - p) if i not in I 
				else  control_set_scores[i] for i in range(N)] 


def multipleRounds(N, n, c, k):

  population_scores, control_set_scores, I, errors = [0.0 for _ in range(N)], [0.0 for _ in range(c)], [], []
  x = genData(N, n)
  traced = set()
  traced_size = []
  for j in range(k):
    p = 0.5
    expected_population_mean = populationMean(N, p)
    def queryOnData(xi):
      return 1.0 if np.random.random_sample() < p and xi >= 0 else 0.0
    def queryOnControlset(ci):
      return 1.0 if np.random.random_sample() < p else 0.0
    a = noNoiseMech(x, queryOnData)
    # update the scores for the population and the control set
    population_scores = [population_scores[i] + (a - p) * ( queryOnData(i) - p) if i not in I 
							else  population_scores[i] for i in range(N)]    
    control_set_scores = [control_set_scores[i] + (a - p) * ( queryOnControlset(i) - p) for i in range(c)]
    # update the traced set
    I = (filter(lambda i: population_scores[i] > max(control_set_scores), range(N) ))
    x = map(lambda x: -1 if x in I else x, x)
    # x = filter(lambda x: (x not in I), x)
    traced = (traced | set(I))
    traced_size.append(len(traced))
    errors.append(abs(a - expected_population_mean))
  return errors

def multipleRoundsQueryDependentDataSplitting(N, n, c, k, B = 10, H = 10, T = 0.25, sigma = 0.035):
  population_scores, control_set_scores, I, errors= [0.0 for _ in range(N)], [0.0 for _ in range(c)], [], []
  # initialize the parameters sepecifically required by the data spliting algorithm
  noised_threshold, batch_counter, B, H  = T + np.random.normal(loc=0.0, scale=sigma, size=None), 1, (n / 10) if B is None else B, (n / H) if H is None else H
  # generate the entire data set, and split it into B batches plus a holdout data set of size H
  x = genData(N, n)
  holdout_data, batches_data = x[:H], [x[H + (i - 1) * B : H + i * B] for i in range((n - H) / B + 1)]
  print(batches_data)
  for j in range(k):
    p = 0.5
    expected_population_mean = populationMean(N, p)
    def queryOnData(xi):
      return 1.0 if np.random.random_sample() < p and xi >= 0 else 0.0
    def queryOnControlset(ci):
      return 1.0 if np.random.random_sample() < p else 0.0
    a, flag = 0.0, True
    # start the loop for the data splitting algorithm
    while flag and batch_counter <= (n - H) / B:
    	a = gaussianMech(batches_data[batch_counter], queryOnData, sigma)
    	a_holdout = noNoiseMech(holdout_data, queryOnData)
    	if a - a_holdout <= noised_threshold:
    		flag = False
    	else:
    		batch_counter  = batch_counter + 1
    		noised_threshold = T + np.random.normal(loc=0.0, scale=sigma, size=None)
    if flag:
    	a = gaussianMech(holdout_data, queryOnData, sigma)

    population_scores = [population_scores[i] + (a - p) * ( queryOnData(i) - p) if i not in I 
							else  population_scores[i] for i in range(N)]
    control_set_scores = [control_set_scores[i] + (a - p) * ( queryOnControlset(i) - p) for i in range(c)]
    I = (filter(lambda i: population_scores[i] > max(control_set_scores), range(N) ))

    if batch_counter <= (n - H) / B:
    	batches_data[batch_counter] = map(lambda x: -1 if x in I else x, batches_data[batch_counter])
    else:
    	holdout_data = map(lambda x: -1 if x in I else x, holdout_data)

    errors.append(abs(a - expected_population_mean))
  return errors

def multipleRoundsDataSplitting(N, n, c, k, B = None, M = None):
  population_scores, control_set_scores, I, errors= [0.0 for _ in range(N)], [0.0 for _ in range(c)], [], []
  # initialize the parameters sepecifically required by the data spliting algorithm
  batch_counter, query_conter_for_each_batch, B = 0, 0, (n / 10) if B is None else B
  M = (k * B / n) if M is None else M
  # generate the entire data set, and split it into B batches plus a holdout data set of size H
  x = genData(N, n)
  batches_data = [x[i * B : (i + 1) * B] for i in range(n / B)]
  for j in range(k):
    p = 0.5
    expected_population_mean = populationMean(N, p)
    def queryOnData(xi):
      return 1.0 if np.random.random_sample() < p and xi >= 0.0 else 0.0
    def queryOnControlset(ci):
      return 1.0 if np.random.random_sample() < p else 0.0
    # start the loop for the data splitting algorithm
    if query_conter_for_each_batch >= M:
    	batch_counter += 1
    	query_conter_for_each_batch = 0
    a = noNoiseMech(batches_data[batch_counter], queryOnData)
    query_conter_for_each_batch = query_conter_for_each_batch + 1    
    population_scores = [population_scores[i] + (a - p) * ( queryOnData(i) - p) if i not in I 
							else  population_scores[i] for i in range(N)]
    control_set_scores = [control_set_scores[i] + (a - p) * ( queryOnControlset(i) - p) for i in range(c)]
    I = (filter(lambda i: population_scores[i] > max(control_set_scores), range(N) ))
    batches_data[batch_counter] = map(lambda x: -1 if x in I else x, batches_data[batch_counter])
    # batches_data[batch_counter] = filter(lambda x: (x not in I), batches_data[batch_counter])
    errors.append(abs(a - expected_population_mean))

  return errors


def multipleRoundsG(N, n, c, k, sigma = 0.35):
  # sigma = getScale(k, n)
  population_scores, control_set_scores, I, errors= [0.0 for _ in range(N)], [0.0 for _ in range(c)], [], []
  x = genData(N, n)
  p_list = [random.random()  for _ in range(k)]
  traced = set()
  traced_size = []
  for j in range(k):
    p = 0.5
    expected_population_mean = populationMean(N, p)
    def queryOnData(xi):
      return 1.0 if np.random.random_sample() < p and xi >= 0 else 0.0
    def queryOnControlset(ci):
      return 1.0 if np.random.random_sample() < p else 0.0
    a = gaussianMech(x, queryOnData, sigma)

    population_scores = [population_scores[i] + (a - p) * ( queryOnData(i) - p) if i not in I 
							else  population_scores[i] for i in range(N)]
    
    # population_scores = updatePopulationScores(population_scores, control_set_scores, a, queryOnData, p, I)

    control_set_scores = [control_set_scores[i] + (a - p) * ( queryOnControlset(i) - p) for i in range(c)]
    I = (filter(lambda i: population_scores[i] > max(control_set_scores), range(N) ))
    x = map(lambda x: -1 if x in I else x, x)
    # x = filter(lambda x: (x not in I), x)
    traced = (traced | set(I))
    traced_size.append(len(traced))
    errors.append(abs(a - expected_population_mean))
  return errors

# N = 5000
# c = 10
# n = 50
# k = 4000

N = 500
c = 20
n = 50
k = 40
sigma = 0.035
runs = 1

def runner(N, n, c, k, runs):
  return sum(np.array([multipleRoundsG(N, n, c, k) for _ in range(runs)])) / (runs * 1.0)


plt.plot(sum(np.array([multipleRoundsDataSplitting(N, n, c, k) for _ in range(runs)])) / (runs * 1.0), 'b', label = "data splitting")
# plt.plot(multipleRoundsQueryDependentDataSplitting(N, n, c, k), label = "0.035 query dependent ds")
plt.plot(sum(np.array([multipleRoundsG(N, n, c, k) for _ in range(runs)])) / (runs * 1.0), 'y', label = "gaussian noise with default scale")
plt.plot(sum(np.array([multipleRoundsG(N, n, c, k, sigma) for _ in range(runs)])) / (runs * 1.0), 'r', label = "gaussian noise")
plt.plot(sum(np.array([multipleRounds(N, n, c, k) for _ in range(runs)])) / (runs * 1.0), 'g', label = "overfitting")
plt.legend()
# plt.title("population(N) {} with {} samples(n) and {} rounds(k)".format(N, n, k))
# plt.title("Data Analysis of Multiple rounds")
plt.grid()
plt.xlabel("Queries")
plt.ylabel("Generalization Error")
plt.ylim(-0.05, 0.8)

# plt.savefig("finalized_plots/multirounds/{}N_{}c_{}n_{}k_gaussian.png".format(N, c, n, k))
plt.savefig("multipleround-test.png")
plt.show()