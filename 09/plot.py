#!/usr/bin/env python

import sys
import numpy as np
import matplotlib.pyplot as plt
from scipy.interpolate import interp1d

filenames = sys.argv[1:]

p_c = 0.593
β = 5 / 36
ν = 4 / 3

p_x = np.linspace(0., 1., 100)

fig, (ax_ql, ax_m_inf) = plt.subplots(1, 2)

# fig.suptitle("Aufgabe 19")
ax_ql.set_xlabel('$p$')
ax_ql.set_ylabel('$q_l$')

ax_m_inf.set_xlabel('$(p - p_c) l^{1/ν}$')
ax_m_inf.set_ylabel('$M_∞ l^{β/ν}$')
for f in filenames:
    p, l, r, ql, m_inf = np.genfromtxt(f, unpack=True)

    p_sorted, ql_sorted = np.sort(np.array([p, ql]))
    ql_spline = interp1d(p, ql, kind='cubic')
    ax_ql.plot(p_sorted, ql_sorted, 'o', p_sorted, ql_spline(p_sorted), '--',
               label="l = {:d}".format(int(l[0])))

    p_sorted, m_inf_sorted = np.sort(np.array([p, m_inf]))
    p_scaled = (p_sorted-p_c) * np.power(l[0], 1 / ν)
    ml_spline = interp1d(p_scaled, m_inf_sorted, kind='cubic')
    ax_m_inf.plot(p_scaled, m_inf_sorted * np.power(l[0], β / ν), 'o',
                  p_scaled, ml_spline(p_scaled), '--',
                  label="l = {:d}".format(int(l[0])))

ax_ql.legend()
ax_m_inf.legend()
fig.tight_layout()
fig.savefig("aufgabe19-2.pdf")
