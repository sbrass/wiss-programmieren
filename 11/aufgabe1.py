#!/usr/bin/env python

import numpy as np
import matplotlib.pyplot as plt

data = np.genfromtxt('aufgabe1.dat')

r = data.T[0:2, :].T
v = data.T[2:4, :].T
h = data.T[4, :].T

i = np.linspace(0, 10, 10001, endpoint=True)

plt.subplot(3, 1, 1)
plt.plot(i, r[:, 0], '--', label='rx')
plt.plot(i, v[:, 0], '-', label='vx')
plt.grid()
plt.title('x-component')
plt.legend()


plt.subplot(3, 1, 2)
plt.plot(i, r[:, 1], '--', label='ry')
plt.plot(i, v[:, 1], '-', label='vy')
plt.grid()
plt.title('y-component')
plt.legend()

plt.subplot(3, 1, 3)
plt.plot(i, h, label='Total energy')
plt.legend()

plt.tight_layout()
plt.savefig('aufgabe1.pdf')
