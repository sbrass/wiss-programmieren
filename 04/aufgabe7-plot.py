#!/usr/bin/env python

import numpy as np
import matplotlib.pyplot as plt


def f1(x):
    return 0.5 * x**3 - 2 * x**2 + x + 1


def f2(x):
    return 3 * x + np.sin(x) - np.exp(x)


x = np.linspace(-5., 5, 100)
plt.ylim(-20, 20)
plt.axhline(linestyle='--', color='black')
plt.plot(x, f1(x))
plt.plot(x, f2(x))

plt.show()
