FC=gfortran
FCFLAGS=-Og -Wall -pedantic -std=f2008ts -fbacktrace -g

all: aufgabe12 aufgabe13

aufgabe%.pdf: aufgabe%.dat
	./plot.py $<

aufgabe%.dat: aufgabe%
	./$< > $@

aufgabe12: func.o func_impl.o integration.o aufgabe12.o
	$(FC) $(FCFLAGS) -o $@ $^

aufgabe13: func.o func_impl.o integration.o aufgabe13.o
	$(FC) $(FCFLAGS) -o $@ $^

%.o: %.f90
	$(FC) $(FCFLAGS) -o $@ -c $<

.PHONY: clean
clean:
	rm -f *.o *.mod *.dat *.pdf
	rm -f aufgabe10 aufgabe11-1 aufgabe11-2
