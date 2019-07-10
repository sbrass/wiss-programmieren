#!/usr/bin/env python

import numpy as np
from scipy import signal
import matplotlib.pyplot as plt
import sys

if len(sys.argv) < 1:
    print("Provide: [FILENAME]")
    exit

filename = sys.argv[1]
π = np.genfromtxt(filename, unpack=True)


def gaussian(x, mu, sig):
    return np.exp(-np.power(x - mu, 2.) / (2 * np.power(sig, 2))) / np.sqrt(2 * np.pi * np.power(sig, 2))


# the histogram of the data
n, bins, patches = plt.hist(π, 25, density=True, label='MC')
x = np.linspace(2.9, 3.4, 100)
σ_π = np.std(π)
plt.plot(x, gaussian(x, np.pi, σ_π), 'k--', label='ZGS')

plt.xlabel('$π$')
plt.legend()

plt.savefig('{}.pdf'.format(filename.split('.')[0]))
