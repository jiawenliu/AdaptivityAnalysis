import matplotlib.pyplot as plt
import glob
import numpy as np

def read_lines(ifile):
	return  [float(line) for line in open(ifile)]

def read_files(dir):
	return [(read_line(file),file) for file in glob.glob(dir)]

def read_lables(dir):
	return glob.glob(dir)

def plot_lines(ys, x):
	for  y,label in enumerate(ys):
		plt.plot(y, x, label=label)
	plt.show()

datas = read_files("results/*.txt")
plot_lines(datas,  np.arange(10, 100, 5) )

for  y,label in enumerate(datas):
	plt.plot(y, np.arange(10, 100, 5), label=label)
plt.show()
