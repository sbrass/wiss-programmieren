#!/usr/bin/env python

import os

VariantDir('build', 'src')
env = Environment(ENV=os.environ,
                  PDFLATEX='lualatex',
                  PDFLATEXFLAGS='--interaction=nonstopmode --halt-on-error --recorder --shell-escape')
# pdfiles = env.PDF (['build/Blatt{}.tex'.format(i) for i in range(1,10)])
pdfiles = env.PDF(['build/Blatt1.tex', 'build/Blatt8.tex'])
# NoClean(pdfiles, '*.pdf')
env.Clean(pdfiles, ['*.log', '*.xml', '*.aux', '*.tex'])
