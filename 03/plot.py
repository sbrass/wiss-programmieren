#!/usr/bin/env python

import sys
import matplotlib.pyplot as plt
import numpy as np

filename = sys.argv[1]

index = filename.split('.')[0].split('-')[-1]
type = {
    '1': 'normal',
    '2': 'normal',
    '3': 'sin',
    '4': 'power'
}

data = np.genfromtxt(filename)

f = plt.figure()

plt.title("{}".format(filename.split('.')[0]))
if type[index] == 'normal':
    bins = np.linspace(-5, 5, 50)
elif type[index] == 'sin':
    bins = np.linspace(0, np.pi, 50)
elif type[index] == 'power':
    bins = np.linspace(0, 1, 50)

n, bins, patches = plt.hist(np.hstack(data), bins, density=True,  label='Data')

if type[index] == 'normal':
    x = np.linspace(-5, 5, 1000)
    plt.plot(x, 1. / np.sqrt(2. * np.pi) * np.exp(-x**2 / 2.))
elif type[index] == 'sin':
    x = np.linspace(0, np.pi, 1000)
    plt.plot(x, 0.5 * np.sin(x))
elif type[index] == 'power':
    x = np.linspace(0, 1, 1000)
    plt.plot(x, 3. * x**2)

f.savefig("{}.pdf".format(filename.split('.')[0]))
