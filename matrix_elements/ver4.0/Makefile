## Makefile to compile in: XEON QUAD ITANIUM
## Last Modification     : Sunday 1 June 2007 23:10 
CWD=$(shell pwd)
PLATAFORM=$(shell hostname)
PRINTF=@printf
ab=ascii
OBJS= sub_pmn_${ab}.o readAbinitWF.o geometry.o

	ifeq  ($(PLATAFORM),medusa)
	F90=/opt/intel/fc/10.1.008/bin/ifort
	FFLAGS= -O3 -xW -static -tune pn4
	NAME=rpmns.xeon
	WHATPL=XEON
	endif

	ifeq    ($(PLATAFORM),itanium01)
	F90 =/opt/intel/fc/9.0/bin/ifort
	FFLAGS = -O3 -parallel -static -tpp2
	NAME=rpmns.itanium
	WHATPL=ITANIUM
	endif

	ifeq    ($(PLATAFORM),quad01)
        F90= /opt/intel/composer_xe_2011_sp1.6.233/bin/intel64/ifort
	#F90 = /opt/intel/fce/10.1.008/bin/ifort
	FFLAGS = -O3  
	NAME=rpmns.quad
	WHATPL=QUAD
	endif

	ifeq    ($(PLATAFORM),hexa2)
        F90=/opt/intel/composer_xe_2011_sp1.6.233/bin/intel64/ifort
	FFLAGS=-O3 -axSSE4.2
#	FFLAGS = -O3  
	NAME=rpmns.hexa
	WHATPL=HEXA
	endif

all: pmn.f90 ${OBJS}
	${F90} ${FFLAGS} -o ${NAME} pmn.f90 ${OBJS}
	rm -f *.o *.mod *~
	$(PRINTF) "\t*********************************\n"
	$(PRINTF) "\tCOMPILED on: $(WHATPL)\n"
	$(PRINTF) "\tOUTPUT     : ../${NAME}\n"
	$(PRINTF) "\t*********************************\n"
sub_pmn_${ab}.o: sub_pmn_${ab}.f90
	${F90} ${FFLAGS} -c sub_pmn_${ab}.f90

geometry.o: geometry.f90
	${F90} ${FFLAGS} -c geometry.f90

readAbinitWF.o: readAbinitWF.f90
	${F90} ${FFLAGS} -c readAbinitWF.f90

clean:
	rm -f *.o;
	rm -f *.mod;

