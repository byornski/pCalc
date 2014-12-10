FC = gfortran

#Runs pretty fast anywway
#FFLAGS  = -fbacktrace -fbounds-check -g -O0 -fimplicit-none -fmax-errors=1 -Wall -ftrapv -fdefault-integer-8 -std=f2003
FFLAGS = -g -O3 -fdefault-integer-8 -std=f2003 -fimplicit-none
LDFLAGS = -static-libgfortran

pCalc: pc.o
	$(FC) -o $@ $(LDFLAGS) $(FFLAGS) *.o

#pc.o pc.mod: pc.f90 tokens.mod operators.mod operatorstack.mod int_stack.mod \
 bigintops.mod
#bigint.o bigintops.mod: bigint.f90 biginttype.mod
#bigintdef.o biginttype.mod: bigintdef.f90
#intstack.o int_stack.mod: intstack.f90 bigintops.mod
#operators.o operators.mod: operators.f90 bigintops.mod
#opstack.o operatorstack.mod: opstack.f90 operators.mod
#token.o tokens.mod: token.f90 operators.mod biginttype.mod

include pCalc.deps


%.o %.mod: 
	$(FC) -c $(FFLAGS) $<



.PHONY: clean cleanall deps dist
dist: deps
deps: pCalc
	./makedeps.sh > pCalc.deps
clean:
	@rm -f *.o *.mod *.obj pCalc
cleanall: clean
	rm -f *~
