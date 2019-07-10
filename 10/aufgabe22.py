#!/usr/bin/env python

import numpy as np
import matplotlib.patches as mpatches
import matplotlib.pyplot as plt
import sys

files = sys.argv[1:4]
β = [0.1, 1, 100]
col = ['r', 'b', 'g']
result = [2000, 2, 2e-6]

for i in range(0, 3):
    print(files[i])
    n, simple, importance, markov = np.genfromtxt(files[i], unpack=True)
    print(i, n)
    simple, = plt.plot(n, abs(simple - result[i]) / result[i], '{}.'.format(col[i]), label="β = {}".format(β[i]))
    importance, = plt.plot(n, abs(importance - result[i]) / result[i], '{}x'.format(col[i]))
    markov, = plt.plot(n, abs(markov - result[i]) / result[i], '{}o'.format(col[i]))


rPatch = mpatches.Patch(color='r')
bPatch = mpatches.Patch(color='b')
gPatch = mpatches.Patch(color='g')

plt.legend([rPatch, bPatch, gPatch, simple, importance, markov],
           ['β = 0.01', 'β = 1', 'β = 100', 'Simple', 'Importance', 'Markov'])

plt.xscale('log')
plt.yscale('log')
plt.xlabel('$n$')
plt.ylabel('$σ$')

plt.savefig('aufgabe22.pdf')
