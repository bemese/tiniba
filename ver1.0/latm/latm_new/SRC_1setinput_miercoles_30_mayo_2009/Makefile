 ## ********************************************
 ## * Warning:                                 
 ## * This Makefile  was generated automatically by:  MakeMakefile.PL 
 ## * Don't edit this file,  edit MakeMakefile.PL instead. 
 ##  *** ANY CHANGES MADE HERE WILL BE LOST! *** 
 ## ********************************************
 ## CREATED on: 2006-09-22 10:26:00  by USER:  bms 
 ## Centro de Investigaciones en Optica A.C. 
 ## Leon, Guanajuato, Mexico 
 ## This is free software;.There is NO warranty. JL
 ##  
CWD=$(shell pwd)
PLATAFORM=$(shell hostname)
ECHO=@echo 
LIBS =	

CC = cc
CFLAGS = -O
FC = f77
FFLAGS =  -O3 -xW -tpp7 
F90 = /opt/intel/fc/9.0/bin/ifort
PROG =	SRC_1setinput

	ifeq	($(PLATAFORM),master)
	PROGA=$(PROG)_xeon
	WHATPL=XEON_32_BITS 
	F90FLAGS = -O3 -tpp7 -xW -static 
	LDFLAGS = -O3 -tpp7 -xW -static 
	endif
	ifeq	($(PLATAFORM),itanium01)
	PROGA=$(PROG)_itanium
	WHATPL=ITANIUM_64_BITS 
	F90FLAGS = -O3 -parallel -static -tpp2 
	LDFLAGS = -O3 -parallel -static -tpp2 
	endif
SRCS =	arrays.f90 constants.f90 file_control.f90 functions.f90 inparams.f90 \
	integrands.f90 set_input_ascii.f90 set_input_bin.f90 \
	symmetry_operations.f90

OBJS =	arrays.o constants.o file_control.o functions.o inparams.o \
	integrands.o set_input_ascii.o set_input_bin.o symmetry_operations.o

$(PROGA): $(OBJS)
	$(F90) $(LDFLAGS) -o $@ $(OBJS) $(LIBS)

	 $(ECHO) " $(WHATPL)  "   
	 $(ECHO) " *********************************** ">> README   
	 $(ECHO) "COMPILED on:$(WHATPL) $(PROGA) ">> README 
	 date >> README 
	 $(ECHO) " *********************************** ">> README 
clean:
	 rm -f $(OBJS) *.mod *~ *.o 

help:
	 $(ECHO) "you are compiling :$(WHATPL)  "   
	 $(ECHO) "  make         -Compila and link "    
	 $(ECHO) "  make help    -Show this info   "    
	 $(ECHO) "  make clean   -clean all *.o *.mod " 
	 $(ECHO) "  make update  -update: /home/bms/abinit_shells/matrix_elements/$(PROGA) " 
	 $(ECHO) "  make cancel  -cancel :/home/bms/abinit_shells/matrix_elements/$(PROGA) " 
update:
	 rm -vf  /home/bms/abinit_shells/matrix_elements/$(PROGA) 
	 cp -vf  /home/bms/abinit_shells/latm_new/SRC_1setinput/$(PROGA) /home/bms/abinit_shells/matrix_elements/$(PROGA) 
	 $(ECHO) " *********************************** " >> README 
	 $(ECHO) "UPDATE :/home/bms/abinit_shells/matrix_elements/$(PROGA)  on">> README 
	 date >> README 
	 $(ECHO) " *********************************** ">> README 
cancel:
	 rm -vf  /home/bms/abinit_shells/matrix_elements/$/(PROGA/)    
	 $(ECHO) " *********************************** ">> README 
	 $(ECHO) "CANCEL  :/home/bms/abinit_shells/matrix_elements/$(PROGA)  on" >> README 
	 date >> README 
	 $(ECHO) " *********************************** " >> README 
.SUFFIXES: $(SUFFIXES) .f90

.f90.o:
	$(F90) $(F90FLAGS) -c $<

arrays.o: constants.o inparams.o
file_control.o: constants.o inparams.o
functions.o: arrays.o constants.o inparams.o
inparams.o: constants.o
integrands.o: arrays.o constants.o functions.o inparams.o
set_input_ascii.o: arrays.o file_control.o functions.o inparams.o \
	integrands.o symmetry_operations.o
set_input_bin.o: arrays.o file_control.o functions.o inparams.o integrands.o \
	symmetry_operations.o
symmetry_operations.o: inparams.o
