FC=gfortran
FCFLAGS=-O2 -Wall -Werror -pedantic -std=f2008ts

aufgabe3: aufgabe3-1 aufgabe3-2 aufgabe3-3 aufgabe3-4
	./aufgabe3-1 > aufgabe3-1.dat && \
	./aufgabe3-2 > aufgabe3-2.dat && \
	./aufgabe3-3 > aufgabe3-3.dat && \
	./aufgabe3-4 > aufgabe3-4.dat && \
	./plot.py aufgabe3-1.dat && \
	./plot.py aufgabe3-2.dat && \
	./plot.py aufgabe3-3.dat
	./plot.py aufgabe3-4.dat

aufgabe4: aufgabe4-1 aufgabe4-2 aufgabe4-3
	./aufgabe4-1 > aufgabe4-1.dat && \
	./aufgabe4-2 > aufgabe4-2.dat && \
	./aufgabe4-3 > aufgabe4-3.dat && \
	./plot.py aufgabe4-1.dat && \
	./plot.py aufgabe4-2.dat && \
	./plot.py aufgabe4-3.dat

aufgabe%-1: rng.o aufgabe%-1.o
	$(FC) $(FCFLAGS) -o $@ $^

aufgabe%-2: rng.o aufgabe%-2.o
	$(FC) $(FCFLAGS) -o $@ $^

aufgabe%-3: rng.o aufgabe%-3.o
	$(FC) $(FCFLAGS) -o $@ $^

aufgabe%-4: rng.o aufgabe%-4.o
	$(FC) $(FCFLAGS) -o $@ $^

rng.o: rng.f90
	$(FC) $(FCFLAGS) -o $@ -c $<

aufgabe3-%.o: aufgabe3-%.f90
	$(FC) $(FCFLAGS) -o $@ -c $<

aufgabe4-%.o: aufgabe4-%.f90
	$(FC) $(FCFLAGS) -o $@ -c $<

.PHONY: clean
clean:
	rm -f *.o *.mod *.dat
	rm -f *.pdf
	rm -f aufgabe3
	rm -f aufgabe4
