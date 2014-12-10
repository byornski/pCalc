FC = gfortran

#Runs pretty fast anywway
#FFLAGS  = -fbacktrace -fbounds-check -g -O0 -fimplicit-none -fmax-errors=1 -Wall -ftrapv -fdefault-integer-8 -std=f2003
FFLAGS = -g -O3 -fdefault-integer-8 -std=f2003 -fimplicit-none
LDFLAGS = -static-libgfortran

src_dir = src
bin_dir = obj

export 


pCalc: $(bin_dir)/deps
	cd $(bin_dir);	$(MAKE)

$(bin_dir)/deps: $(src_dir)/*.f90 $(src_dir)/makedeps.sh
	$(src_dir)/makedeps.sh > $@






.PHONY: clean cleanall 

clean:
	cd $(bin_dir); rm -f *.mod *.o deps
	rm -f pCalc
cleanall: clean
	cd $(src_dir); rm -f *~
