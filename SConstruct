#!/usr/bin/env python

import os

VariantDir('build', 'src')
env = Environment(ENV=os.environ,
                  PDFLATEX='lualatex',
                  PDFLATEXFLAGS='--interaction=nonstopmode --halt-on-error --recorder --shell-escape')
pdfiles = env.PDF (['build/Blatt{}.tex'.format(i) for i in [1, 2, 8]])
env.Clean(pdfiles, ['*.log', '*.xml', '*.aux', '*.tex'])
