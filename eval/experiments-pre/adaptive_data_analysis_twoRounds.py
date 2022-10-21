import math
import numpy as np
import matplotlib as mpl
# mpl.use('tkagg')
import matplotlib.pyplot as plt
import random

def get_scale(k, n):
	return (math.sqrt(math.sqrt(2.0 / math.log(k) )/ n))

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


def genCorrelation(x, n, k, p = 0.5):
  for i in range (n):
     for j in range (k - 1):
       	x[i][j] = x[i][k - 1] if np.random.random_sample() > (1.0 - p) else x[i][j]
  return x

def twoRounds(n, k, p = 0.5):
  x = np.random.randint(2, size=(n,k))
  x = signM(x, n, k)
  x = genCorrelation(x, n, k, p)
  print(x)
  ar = [0]*(k);
  i = 0;
  for j in range (k-1):
    a = 0.0
    for i in range (n):
        a = a + (x[i][j]*x[i][k-1])
    ar[j] = a / (n * 1.0)
  b = 0.0  
  for i in range (n):
    a = 0.0
    for j in range (k):
      if ar[j] == 1.0:
      	a = a + float('inf')
      	continue
      a = a + (x[i][j] * x[i][k - 1] * math.log(abs((1.0 + ar[j])/(1.0 - ar[j]))))
    b = b + (sign(a) * 1.0)
  ar[k-1] = b / (n * 1.0)
  return (ar)

def twoRoundsG(n, k, p = 0.5, sigma = 0.35): 
  x = np.random.randint(2,size=(n,k))
  x = signM(x, n, k)
  x = genCorrelation(x, n, k, p)
  print(x)
  # the default value for the scale of gaussian mechanism
  # sigma = 0.08
  # sigma = get_scale(k, n)
  ar = [0]*(k);
  i = 0;
  for j in range (k-1):
    a = 0.0
    for i in range (n):
        a = a + (x[i][j]*x[i][k-1])
    ar[j] = a / (n * 1.0) + np.random.normal(loc=0.0, scale=sigma, size=None)
  b = 0.0 
  for i in range (n):
    a = 0.0
    for j in range (k):
      a = a + (x[i][j] * x[i][k - 1] * math.log(abs((1.0 + ar[j])/(1.0 - ar[j]))))
    b = b + (sign(a) * 1.0)
  ar[k - 1] = b / (n * 1.0) + np.random.normal(loc=0.0, scale=sigma, size=None)
  return (ar)


def twoRoundsDataSplitting(n, k, p = 0.5):
  n = n / 2
  x = np.random.randint(2, size=(n,k))
  x = signM(x, n, k)
  x = genCorrelation(x, n, k, p)
  print(x)
  ar = [0]*(k);
  i = 0;
  for j in range (k-1):
    a = 0.0
    for i in range (n):
        a = a + (x[i][j]*x[i][k-1])
    ar[j] = a / (n * 1.0)
  x = np.random.randint(2, size=(n,k))
  x = signM(x, n, k)
  b = 0.0 
  for i in range (n):
    a = 0.0
    for j in range (k):
      if ar[j] == 1.0:
      	a = a + float('inf')
      	continue
      a = a + (x[i][j] * x[i][k - 1] * math.log(abs((1.0 + ar[j])/(1.0 - ar[j]))))
    b = b + (sign(a) * 1.0)
  ar[k-1] = b / (n * 1.0)
  return (ar)

def error(p, ar):
	return np.abs(np.append(np.array(ar[:-1]) - p, (ar[-1] - 0.0)))

p = 0.0
n = 500
k = 400
plt.plot(error(p, twoRoundsDataSplitting(n, k, p)), 'b')
plt.plot(error(p, twoRoundsG(n, k, p)), 'r--')
plt.plot(error(p, twoRoundsG(n, k, p, 0.08)), 'y')
plt.plot(error(p, twoRounds(n, k, p)), 'g')

plt.plot([k], error(p, twoRoundsDataSplitting(n, k, p))[-1], 'b^', label = "data splitting")
plt.plot([k], error(p, twoRoundsG(n, k, p))[-1], 'rs', label = "gaussian noise with default scale")
plt.plot([k], error(p, twoRoundsG(n, k, p, 0.08))[-1], 'ys',
	label = "gaussian noise")
plt.plot([k], error(p, twoRounds(n, k, p))[-1], 'g*', label = "overfitted")
# plt.title("Data Analysis of Two rounds data analysis strategy")
# Add labels

plt.xlabel("Queries")
plt.ylabel("Generalization Error")
# plt.title("correlation {}, {} rows (n) and {} attribute (k) with gaussian mech".format(p, n, k))
plt.legend()
plt.grid()
# plt.xlim(, k+10)
plt.ylim(-0.1, 0.8)



# plt.savefig("finalized_plots/mechs/correlation{}_{}n_{}k_DS.png".format(p, n, k))
# plt.savefig("finalized_plots/mechs/tworound.png")
plt.savefig("tworound-test.png")
plt.show()