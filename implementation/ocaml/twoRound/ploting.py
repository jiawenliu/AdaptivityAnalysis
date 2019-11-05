import matplotlib.pyplot as plt
import glob
import numpy as np

def read_lines(ifile):
	return  [float(line) for line in open(ifile)]

def read_files(dir):
	return [(read_lines(file),file) for file in glob.glob(dir)]

def read_lables(dir):
	return glob.glob(dir)

def plot_lines(ys, x):
	for y,label in (ys):
		print y
		plt.plot(x, y, label=label)
	plt.grid(True)
	plt.xlabel("k")
	plt.ylabel("error")
	plt.legend(loc="best")
	plt.show()

datas = read_files("results/non5000row.txt")
print datas
plot_lines(datas,  np.arange(0, 100, 1) )

# for  y,label in enumerate(datas):
# 	plt.plot(y, np.arange(10, 100, 5), label=label)
# plt.show()
