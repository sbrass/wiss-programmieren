#!/usr/bin/env python

import sys
import numpy as np
import matplotlib.pyplot as plt

filename = sys.argv[1]

data = np.genfromtxt(filename)

f = plt.figure()

plt.title(filename.split('.')[0])
plt.plot(data[:, 0], data[:, 1], '.', markersize=0.5, label='naive')
plt.plot(data[:, 0], data[:, 2], '.', markersize=0.5, label='two-symm')
plt.plot(data[:, 0], data[:, 3], '.', markersize=0.5, label='four-symm')

plt.xlabel(r'$h$')
plt.ylabel(r'$f^{\prime}$')
plt.xscale('log')
plt.legend()

f.savefig("{}.pdf".format(filename.split('.')[0]))
