#!/usr/bin/env python

import sys
import numpy as np
import matplotlib.pyplot as plt

filename = sys.argv[1]

cluster = np.genfromtxt(filename)

fig, ax = plt.subplots()
im = ax.imshow(cluster, cmap="magma_r")

# Loop over data dimensions and create text annotations.
# for i in range(len(cluster[:, 1])):
#     for j in range(len(cluster[1, :])):
#         text = ax.text(j, i, "{:d}".format(int(cluster[i, j])),
#                        ha="center", va="center", color="w")

fig.tight_layout()
fig.savefig('{}.pdf'.format(filename.split('.')[0]))
