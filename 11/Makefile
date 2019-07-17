FC=gfortran
FCFLAGS=-O2 -Wall -Werror -pedantic -std=f2008ts -fbacktrace -g

all: aufgabe1.pdf

aufgabe1.pdf: aufgabe1.dat
	./aufgabe1.py $<

aufgabe%.dat: aufgabe%
	./$< > $@

aufgabe1: solver.o harmonic.o aufgabe1.o
	$(FC) $(FCFLAGS) -o $@ $^

%.o: %.f90
	$(FC) $(FCFLAGS) -o $@ -c $<

.PHONY: clean
clean:
	rm -f *.o *.mod *.dat *.pdf
	rm -f aufgabe1
