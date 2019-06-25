FC=gfortran
FCFLAGS=-Og -Wall -pedantic -std=f2008ts -fbacktrace -g -fcheck=all

all: aufgabe16 aufgabe17

-include Makefile.deps
Makefile.deps:
	$(FC) $(FCFLAGS) -cpp -M *.f90 2>/dev/null > Makefile.deps

aufgabe16: polynom_ut.o polynom.o unit_test.o aufgabe16.o
	$(FC) $(FCFLAGS) -o $@ $^

aufgabe17: polynom.o divisor.o long_division.o long_division_ut.o unit_test.o aufgabe17.o
	$(FC) $(FCFLAGS) -o $@ $^

# $@ holds name of the target which invoked the rule, thus need not to be *.o.
# Object file and source-code file have the same name (*.mod files may divert).
%.o %.mod: %.f90
	$(FC) $(FCFLAGS) -o $(<:.f90=.o) -c $<

.PHONY: clean
clean:
	rm -f *.o *.mod *.dat *.pdf
	rm -f aufgabe16 aufgabe17
	rm -f Makefile.deps
