#!/usr/bin/env python

import os

VariantDir('build', 'src')
env = Environment(ENV=os.environ,
                  PDFLATEX='lualatex',
                  PDFLATEXFLAGS='--interaction=nonstopmode --recorder --shell-escape')
pdffiles = env.PDF (['build/Blatt{}.tex'.format(i) for i in [1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11]])
# pdffiles = env.PDF(['build/Blatt1.tex', 'build/Blatt2.tex'])
env.Clean(pdffiles, ['*.log', '*.xml', '*.aux', '*.tex'])
