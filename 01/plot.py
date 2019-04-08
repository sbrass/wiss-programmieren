#!/usr/bin/env python

import numpy as np
import matplotlib.pyplot as plt

data = np.genfromtxt('aufgabe2.dat')

f = plt.figure()
bins = [x + 0.5 for x in np.arange(0, 10, 1)]
n, bins, patches = plt.hist(data, bins, density=True,  label='Data')

x = np.arange(1, 10, 1)
plt.plot(x, np.log((x + 1) / x) / np.log(10), 'kx', label='Benford')

plt.tight_layout()
f.savefig("aufgabe.pdf")
