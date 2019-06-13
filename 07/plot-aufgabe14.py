#!/usr/bin/env python

import numpy as np
import matplotlib.pyplot as plt
import sys

filename = sys.argv[1]

x, y_num, y_ana = np.genfromtxt(filename, unpack=True)

plt.title('Diffraction @Ring')
plt.xlabel('$q$')
plt.ylabel('$I(q)$')
plt.plot(x, y_num, 'ro', label='Numeric')
plt.plot(x, y_ana, 'k-', label='Analytic')
plt.legend()

plt.savefig('{}.pdf'.format(filename.split('.')[0]))
