FC=gfortran
FCFLAGS=-O2 -Wall -Werror -pedantic -std=f2008ts

all: pq aufgabe1 aufgabe2 plot

pq: pq.f90
	$(FC) $(FCFLAGS) -o $@ $<

aufgabe1: aufgabe1.f90
	$(FC) $(FCFLAGS) -o $@ $<

aufgabe2: aufgabe2.f90
	$(FC) $(FCFLAGS) -o $@ $<

aufgabe2.dat: aufgabe2
	./aufgabe2 | cut -d'.' -f1 > aufgabe2.dat

plot: aufgabe2.dat
	./plot.py

.PHONY: clean
clean:
	rm -f pq
	rm -f aufgabe1
	rm -f aufgabe2
	rm -f aufgabe2.dat
	rm -f aufgabe.pdf
