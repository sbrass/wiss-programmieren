#!/usr/bin/env python

import os

VariantDir('build', 'src')
env = Environment(ENV=os.environ,
                  PDFLATEX='lualatex',
                  PDFLATEXFLAGS='--interaction=nonstopmode')
# pdfiles = env.PDF (['build/Blatt{}.tex'.format(i) for i in range(1,10)])
pdfiles = env.PDF(['build/Blatt1.tex'])
# NoClean(pdfiles, '*.pdf')
env.Clean(pdfiles, ['*.log', '*.xml', '*.aux', '*.tex'])
