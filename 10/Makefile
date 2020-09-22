FC=gfortran
FCFLAGS=-O2 -Wall -Werror -pedantic -std=f2008ts -fbacktrace -g

all: aufgabe21-1.pdf aufgabe21-2.pdf aufgabe21-3 aufgabe22.pdf

aufgabe21-1.pdf: aufgabe21-1.dat
	./aufgabe21-1.py $<

aufgabe21-2.pdf: aufgabe21-2.dat
	./aufgabe21-2.py $<

aufgabe22.pdf: aufgabe22-1.dat aufgabe22-2.dat aufgabe22-3.dat
	./aufgabe22.py $^

aufgabe%.dat: aufgabe%
	./$< > $@

aufgabe22-1.dat: aufgabe22
	./$< 7 0.1 > $@

aufgabe22-2.dat: aufgabe22
	./$< 7 1 > $@

aufgabe22-3.dat: aufgabe22
	./$< 7 100 > $@

aufgabe21-1: aufgabe21-1.o
	$(FC) $(FCFLAGS) -o $@ $^

aufgabe21-2: aufgabe21-2.o
	$(FC) $(FCFLAGS) -o $@ $^

aufgabe21-3: aufgabe21-3.o
	$(FC) $(FCFLAGS) -o $@ $^

aufgabe22: importance.o aufgabe22.o
	$(FC) $(FCFLAGS) -o $@ $^

%.o: %.f90
	$(FC) $(FCFLAGS) -o $@ -c $<

.PHONY: clean
clean:
	rm -f *.o *.mod *.dat *.pdf
	rm -f aufgabe21-1 aufgabe21-2 aufgabe21-3 aufgabe22
