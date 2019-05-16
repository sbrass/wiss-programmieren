FC=gfortran
FCFLAGS=-O2 -Wall -Werror -pedantic -std=f2008ts -fbacktrace -g

all: aufgabe10.pdf aufgabe11-1.pdf aufgabe11-2

aufgabe%.pdf: aufgabe%.dat
	./plot.py $<

aufgabe%.dat: aufgabe%
	./$< > $@

aufgabe11-1.pdf: aufgabe11-1.dat
	split -a 1 -d -l 1001 aufgabe11-1.dat aufgabe11-1- --additional-suffix .dat
	./plot-aufgabe11.py

aufgabe10: func.o polynom.o derivatives.o aufgabe10.o
	$(FC) $(FCFLAGS) -o $@ $^

aufgabe11-1: func.o hermite.o derivatives.o aufgabe11-1.o
	$(FC) $(FCFLAGS) -o $@ $^

aufgabe11-2: func.o hermite.o derivatives.o aufgabe11-2.o
	$(FC) $(FCFLAGS) -o $@ $^

%.o: %.f90
	$(FC) $(FCFLAGS) -o $@ -c $<

.PHONY: clean
clean:
	rm -f *.o *.mod *.dat *.pdf
	rm -f aufgabe10 aufgabe11-1 aufgabe11-2
