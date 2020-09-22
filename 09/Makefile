FC=gfortran
FCFLAGS=-Og -Wall -pedantic -std=f2008ts -fbacktrace -g

all: aufgabe19-1-1.pdf aufgabe19-1-2.pdf aufgabe19-1-3.pdf aufgabe19-2.pdf aufgabe20-1-1.pdf aufgabe20-1-2.pdf aufgabe20-1-3.pdf
#all: aufgabe20-1-1.pdf aufgabe20-1-2.pdf aufgabe20-1-3.pdf

aufgabe19-%.pdf: aufgabe19-%.dat
	./plot-cluster.py $<

aufgabe20-%.pdf: aufgabe20-%.dat
	./plot-cluster.py $<

aufgabe19-2.pdf: aufgabe19-2-10.dat aufgabe19-2-50.dat aufgabe19-2-100.dat
	./plot.py $^

aufgabe%-1-1.dat: aufgabe%-1
	./$< 1 50 0.1 > $@

aufgabe%-1-2.dat: aufgabe%-1
	./$< 1 50 0.5 > $@

aufgabe%-1-3.dat: aufgabe%-1
	./$< 1 50 0.9 > $@

aufgabe19-2-10.dat: aufgabe19-2
	parallel -j3 ./$< {1} {2} {3} ::: 10000 ::: 10 ::: 0.1 0.15 0.2 0.25 0.3 0.35 0.4 0.45 0.5 0.55 0.575 0.6 0.625 0.65 0.7 0.75 0.8 0.85 0.9 0.95 1.0 > $@

aufgabe19-2-50.dat: aufgabe19-2
	parallel -j3 ./$< {1} {2} {3} ::: 500 ::: 50 ::: 0.1 0.15 0.2 0.25 0.3 0.35 0.4 0.45 0.5 0.55 0.575 0.6 0.625 0.65 0.7 0.75 0.8 0.85 0.9 0.95 1.0 > $@

aufgabe19-2-100.dat: aufgabe19-2
	parallel -j3 ./$< {1} {2} {3} ::: 500 ::: 100 ::: 0.1 0.15 0.2 0.25 0.3 0.35 0.4 0.45 0.5 0.55 0.575 0.6 0.625 0.65 0.7 0.75 0.8 0.85 0.9 0.95 1.0 > $@

aufgabe19-1: list.o cluster.o aufgabe19-1.o
	$(FC) $(FCFLAGS) -o $@ $^

aufgabe19-2: list.o cluster.o aufgabe19-2.o
	$(FC) $(FCFLAGS) -o $@ $^

aufgabe20-1: list.o cluster.o aufgabe20-1.o
	$(FC) $(FCFLAGS) -o $@ $^

%.o: %.f90
	$(FC) $(FCFLAGS) -o $@ -c $<

.PHONY: clean
clean:
	rm -f *.o *.mod *.dat *.pdf
	rm -f aufgabe19-1 aufgabe19-2
