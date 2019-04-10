FC=gfortran
FCFLAGS=-O2 -Wall -Werror -pedantic -std=f2008ts

aufgabe3: rng.o aufgabe3.o
	$(FC) $(FCFLAGS) -o $@ $^

aufgabe4: rng.o aufgabe4.o
	$(FC) $(FCFLAGS) -o $@ $^

rng.o: rng.f90
	$(FC) $(FCFLAGS) -o $@ -c $<

aufgabe3.o: aufgabe3.f90
	$(FC) $(FCFLAGS) -o $@ -c $<

aufgabe4.o: aufgabe4.f90
	$(FC) $(FCFLAGS) -o $@ -c $<

.PHONY: clean
clean:
	rm -f *.o *.mod *.dat
	rm -f aufgabe3
	rm -f aufgabe4
