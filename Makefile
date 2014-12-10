FC = gfortran

#Runs pretty fast anywway
#FFLAGS  = -fbacktrace -fbounds-check -g -O0 -fimplicit-none -fmax-errors=1 -Wall -ftrapv -fdefault-integer-8 -std=f2003
FFLAGS = -g -O3 -fdefault-integer-8 -std=f2003 -fimplicit-none
LDFLAGS = -static-libgfortran

pCalc: pc.o
	$(FC) -o $@ $(LDFLAGS) $(FFLAGS) *.o

include pCalc.deps


%.o %.mod: 
	$(FC) -c $(FFLAGS) $<



.PHONY: clean cleanall deps dist
dist: deps
deps: pCalc
	./makedeps.sh > pCalc.deps
clean:
	rm -f *.o *.mod *.obj pCalc
cleanall: clean
	rm -f *~
