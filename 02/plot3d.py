#!/usr/bin/env python

import sys
from mpl_toolkits.mplot3d import axes3d

import matplotlib.pyplot as plt
import numpy as np

filename = sys.argv[1]

data = np.genfromtxt(filename)

f = plt.figure()
ax = f.add_subplot(111, projection='3d')

plt.suptitle("{}".format(filename.split('.')[0]))

x, y, z = np.split(np.hstack(data)[:-2], 3)
ax.scatter(x, y, z)

f.savefig("{}-3d.pdf".format(filename.split('.')[0]))
