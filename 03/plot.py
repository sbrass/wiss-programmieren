#!/usr/bin/env python

import sys
import matplotlib.pyplot as plt
import numpy as np

filename = sys.argv[1]

data = np.genfromtxt(filename)

f = plt.figure()

plt.title("{}".format(filename.split('.')[0]))

bins = np.linspace(-5, 5, 50)
n, bins, patches = plt.hist(np.hstack(data), bins, density=True,  label='Data')

x = np.linspace(-5, 5, 1000)
plt.plot(x, 1. / np.sqrt(2. * np.pi) * np.exp(-x**2 / 2.))

f.savefig("{}.pdf".format(filename.split('.')[0]))
