#!/usr/bin/env python

import numpy as np
import matplotlib.pyplot as plt
import sys

if (len(sys.argv) < 2):
    print("./aufgabe1.py [FILENAME]")
    print("Produces a PDF.")
    print("File-Format: tₙ rₙ vₙ Eₙ mit d=2.")
    exit(1)

filename = sys.argv[1]
print("Open and parse {}.".format(filename))
data = np.genfromtxt(filename)

t = data.T[0, :].T
r = data.T[1:3, :].T
v = data.T[3:5, :].T
h = data.T[5, :].T

plt.subplot(3, 1, 1)
plt.plot(t, r[:, 0], '--', label='rx')
plt.plot(t, v[:, 0], '-', label='vx')
plt.grid()
plt.title('x-component')
plt.legend()


plt.subplot(3, 1, 2)
plt.plot(t, r[:, 1], '--', label='ry')
plt.plot(t, v[:, 1], '-', label='vy')
plt.grid()
plt.title('y-component')
plt.legend()

plt.subplot(3, 1, 3)
plt.plot(t, h, label='Total energy')
plt.legend()

plt.tight_layout()
print("Save to {}.pdf.".format(filename.split('.')[0]))
plt.savefig('{}.pdf'.format(filename.split('.')[0]))
