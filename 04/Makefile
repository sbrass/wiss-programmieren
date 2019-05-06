FC=gfortran
FCFLAGS=-O2 -Wall -Werror -pedantic -std=f2008ts

all: aufgabe7 aufgabe8-1.pdf aufgabe8-2.pdf aufgabe9-1 aufgabe9-2

aufgabe%.pdf: aufgabe%.dat
	./plot.py $<

aufgabe%.dat: aufgabe%
	./$< > $@

aufgabe%: aufgabe%.o
	$(FC) $(FCFLAGS) -o $@ $^

%.o: %.f90
	$(FC) $(FCFLAGS) -o $@ -c $<

.PHONY: clean
clean:
	rm -f *.o *.mod *.dat *.pdf
	rm -f aufgabe7 aufgabe8-1 aufgabe8-2 aufgabe9-1 aufgabe9-2
