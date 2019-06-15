FC=gfortran
FCFLAGS=-Og -Wall -pedantic -std=f2008ts -fbacktrace -g -fcheck=all

all: aufgabe16

aufgabe16: polynom.o unittest.o polynom_ut.o aufgabe16.o
	$(FC) $(FCFLAGS) -o $@ $^

aufgabe14-2: func.o beugung.o integration.o aufgabe14-2.o
	$(FC) $(FCFLAGS) -o $@ $^

aufgabe15: func.o elektrostatik.o integration.o aufgabe15.o
	$(FC) $(FCFLAGS) -o $@ $^

%.o: %.f90
	$(FC) $(FCFLAGS) -o $@ -c $<

.PHONY: clean
clean:
	rm -f *.o *.mod *.dat *.pdf
	rm -f aufgabe14-1 aufgabe14-2 aufgabe15
