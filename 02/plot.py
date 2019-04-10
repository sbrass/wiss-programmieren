#!/usr/bin/env python

import sys
import matplotlib.pyplot as plt
import numpy as np

filename = sys.argv[1]

data = np.genfromtxt(filename)

f = plt.figure()

plt.suptitle("{}".format(filename.split('.')[0]))
plt.subplot(2, 1, 1)

bins = np.linspace(0, 1, 10)
n, bins, patches = plt.hist(np.hstack(data), bins, density=True,  label='Data')

plt.subplot(2, 1, 2)

x = data.T[0]
y = data.T[1]
plt.plot(x[:1000], y[:1000], ',')

f.savefig("{}.pdf".format(filename.split('.')[0]))
