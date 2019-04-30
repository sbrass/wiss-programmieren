FC=gfortran
FCFLAGS=-O2 -Wall -Werror -pedantic -std=f2008ts

all: aufgabe6-1.pdf aufgabe6-2.pdf aufgabe6-3.pdf aufgabe6-4.pdf aufgabe7-1 aufgabe7-2

aufgabe%.pdf: aufgabe%.dat
	./plot.py $<

aufgabe%.dat: aufgabe%
	./$< > $@

aufgabe%: rng.o aufgabe%.o
	$(FC) $(FCFLAGS) -o $@ $^

%.o: %.f90
	$(FC) $(FCFLAGS) -o $@ -c $<

.PHONY: clean
clean:
	rm -f *.o *.mod *.dat *.pdf
	rm -f aufgabe6-1 aufgabe6-2 aufgabe6-3 aufgabe6-4 aufgabe7-1 aufgabe7-2
