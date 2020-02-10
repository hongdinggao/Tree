FORTRAN=ifort
FC = -O3 -traceback  # only works for ifort
#FC = -O3
SRC :=
kernel=$(shell uname -r)
TOOLS = $(Lib_Tools_1.1)
ToolFlag = $(TOOLS)Lib_Tools_$(FORTRAN)_$(kernel)_1.1.a
MKL=$(MKLPARA)
.SUFFIXES:
.SUFFIXES: .f90

include files.mk

OBJS = $(subst .f90,.o,$(SRC))

%.o: %.f90
	$(FORTRAN) $(FC) -c -o $@ $<

test: $(OBJS)
	$(FORTRAN) $(FC) -o $@ $^


clean:
	-rm test
	-rm *.o *.mod *.smod
