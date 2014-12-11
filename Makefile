

ifeq ($(FC),f77)
#make likes f77 as a default which clearly will not build f2003 code
	FC = gfortran
endif

ifeq ($(FC),gfortran)
	FFLAGS = -g -O3 -fdefault-integer-8 -std=f2003 -fimplicit-none
	LDFLAGS = -static-libgfortran	
else	
#Casually assume this is ifort....
	FFLAGS = -g -O3 -implicitnone -i8
endif


#Runs pretty fast anywway
#FFLAGS  = -fbacktrace -fbounds-check -g -O0 -fimplicit-none -fmax-errors=1 -Wall -ftrapv -fdefault-integer-8 -std=f2003


src_dir = src
bin_dir = obj

export 


pCalc: $(bin_dir)/deps
	cd $(bin_dir);	$(MAKE)

$(bin_dir)/deps: $(src_dir)/*.f90 $(src_dir)/makedeps.sh
	$(src_dir)/makedeps.sh > $@






.PHONY: clean cleanall 

clean:
	cd $(bin_dir); find . -not -name "Makefile" -type f | xargs rm -f
	rm -f pCalc
cleanall: clean
	cd $(src_dir); rm -f *~
