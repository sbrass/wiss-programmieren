FC=gfortran
FCFLAGS=-Og -Wall -pedantic -std=f2008ts -fbacktrace -g

all: aufgabe14-1.pdf aufgabe14-2.pdf aufgabe15-1

aufgabe14-1.pdf: aufgabe14-1.dat
	./plot-aufgabe14.py $<

aufgabe14-2.pdf: aufgabe14-2.dat
	./plot-aufgabe14-2d.py $<

aufgabe15.pdf: aufgabe15.dat
	./plot-aufgabe15.py $<

aufgabe%.dat: aufgabe%
	./$< > $@

aufgabe14-1: func.o beugung.o integration.o aufgabe14-1.o
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
