FC=gfortran
FCFLAGS=-O2 -Wall -Werror -pedantic -std=f2008ts -fbacktrace -g

all: aufgabe10.pdf aufgabe11.pdf

aufgabe%.pdf: aufgabe%.dat
	./plot.py $<

aufgabe%.dat: aufgabe%
	./$< > $@

aufgabe11.pdf: aufgabe11.dat
	split -a 1 -d -l 1001 aufgabe11.dat aufgabe11- --additional-suffix .dat
	./plot-aufgabe11.py

aufgabe10: func.o polynom.o derivatives.o aufgabe10.o
	$(FC) $(FCFLAGS) -o $@ $^

aufgabe11: func.o hermite.o derivatives.o aufgabe11.o
	$(FC) $(FCFLAGS) -o $@ $^

%.o: %.f90
	$(FC) $(FCFLAGS) -o $@ -c $<

.PHONY: clean
clean:
	rm -f *.o *.mod *.dat *.pdf
	rm -f aufgabe10 aufgabe11
