#!/usr/bin/env python

import numpy as np
import matplotlib.pyplot as plt
import sys

filename = sys.argv[1]

x, y, z = np.genfromtxt(filename, unpack=True)

heatmap, xedges, yedges = np.histogram2d(x, y, weights=z, bins=20)
extent = [xedges[0], xedges[-1], yedges[0], yedges[-1]]

plt.title('Diffraction @Quarter-Ring')
plt.imshow(heatmap.T, extent=extent, origin='lower')
plt.xlabel('$q_x$')
plt.ylabel('$q_y$')

plt.savefig('{}.pdf'.format(filename.split('.')[0]))
