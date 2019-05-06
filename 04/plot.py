#!/usr/bin/env python

import sys
import numpy as np
import matplotlib.pyplot as plt

filename = sys.argv[1]

data = np.genfromtxt(filename)

f = plt.figure()

plt.title(filename.split('.')[0])
plt.plot(data[:, 0], data[:, 1:-1], 'k.', markersize=0.1)
plt.xlabel('r')
plt.ylabel('x')

f.savefig("{}.pdf".format(filename.split('.')[0]))
