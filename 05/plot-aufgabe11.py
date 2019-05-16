#!/usr/bin/env python

import numpy as np
import matplotlib.pyplot as plt
from matplotlib.backends.backend_pdf import PdfPages


filename = "aufgabe11-1"
label = ['1', '2', '5', '42']

pp = PdfPages('{}.pdf'.format(filename))

plt.title("Hermite-Polynome")
for i in [0, 1, 2]:
    data = np.genfromtxt("{}-{}.dat".format(filename, i))
    plt.plot(data[:, 0], data[:, 1], '-.', markersize=0.5, label="n = {}".format(label[i]))

plt.xlabel(r'$x$')
plt.ylabel(r'$H(x)$')
plt.legend()

pp.savefig()
plt.clf()

plt.title("Wellenfunktionen")
for i in [0, 1, 2]:
    data = np.genfromtxt("{}-{}.dat".format(filename, i))
    plt.plot(data[:, 0], data[:, 2], '--', label='n = {}'.format(label[i]))

plt.xlabel(r'$x$')
plt.ylabel(r'$Ψ_n(x)$')
plt.legend()
pp.savefig()

pp.close()
