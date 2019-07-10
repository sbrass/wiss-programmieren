#!/usr/bin/env python

import numpy as np
import matplotlib.pyplot as plt
import sys

if len(sys.argv) < 1:
    print("Provide: [FILENAME]")
    exit

filename = sys.argv[1]
n, σ = np.genfromtxt(filename, unpack=True)

plt.plot(n, σ, 'ro', label='MC')
plt.plot(n, 1 / np.sqrt(n - 1), '--', label='$1/√(n - 1)$')

plt.xscale('log')
plt.yscale('log')
plt.xlabel('$n$')
plt.ylabel('$σ$')
plt.legend()

plt.savefig('{}.pdf'.format(filename.split('.')[0]))
