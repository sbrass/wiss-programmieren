FC=gfortran
FCFLAGS=-O2 -Wall -Werror -pedantic -std=f2008ts

aufgabe3: aufgabe3.o rng.o
	$(FC) $(FCFLAGS) -o $@ $^

aufgabe4: aufgabe4.o rng.o
	$(FC) $(FCFLAGS) -o $@ $^

rng.o: rng.f90
	$(FC) $(FCFLAGS) -o $@ -c $<

aufgabe3.o: aufgabe3.f90
	$(FC) $(FCFLAGS) -o $@ -c $<

aufgabe4.o: aufgabe4.f90
	$(FC) $(FCFLAGS) -o $@ -c $<

.PHONY: clean
clean:
	rm -f *.o
	rm -f aufgabe3
