#!/usr/bin/env python

import numpy as np
import matplotlib.pyplot as plt
import sys

filename = sys.argv[1]

x, φ_stat_num, φ_stat_mono, φ_dyn_num, φ_dyn_dipol = np.genfromtxt(filename,
                                                                   unpack=True)

plt.subplot(1, 2, 1)
plt.plot(x, φ_stat_num, 'r-', label='numeric')
plt.plot(x, φ_stat_mono, 'g-', label='mono')
plt.title('Static Charge Density')
plt.xlabel(r'$\frac{x}{a}$')
plt.ylabel(r'$φ / (\frac{ρ_0 a^2}{4πε_0})$')
plt.ylim(bottom=0, top=12)
plt.legend()

plt.subplot(1, 2, 2)
plt.plot(x, φ_dyn_num, 'r-', label='numeric')
plt.plot(x, φ_dyn_dipol, 'g-', label='dipol')
plt.title('Dynamic Charge Density')
plt.xlabel(r'$\frac{x}{a}$')
plt.ylabel(r'$φ / (\frac{ρ_0 a^2}{4πε_0})$')
plt.ylim(bottom=0, top=2)
plt.legend()

plt.tight_layout()
plt.savefig('{}.pdf'.format(filename.split('.')[0]))
