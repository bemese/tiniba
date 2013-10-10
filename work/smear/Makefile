 ## ********************************************
 ## * Warning:                                 
 ## * This Makefile  was generated automatically by:  MakeMakefile.PL 
 ## * Don't edit this file,  edit MakeMakefile.PL instead. 
 ##  *** ANY CHANGES MADE HERE WILL BE LOST! *** 
 ## ********************************************
 ## CREATED on: 2010-11-22 16:58:54  by USER:  bms 
 ## Centro de Investigaciones en Optica A.C. 
 ## Leon, Guanajuato, Mexico 
 ## This is free software;.There is NO warranty. JL
 ##  
	COMPILER-XEON = /opt/intel/fc/10.1.008/bin/ifort
	COMPILER-ITAN = /opt/intel/fc/9.1.036/bin/ifort
	COMPILER-QUAD = /opt/intel/fce/10.1.008/bin/ifort
	COMPILER-HEXA = /opt/intel/composer_xe_2011_sp1.6.233/bin/intel64/ifort
	   FLAGS-XEON = -O3  -xW 
	   FLAGS-ITAN = -O3 -tpp2
	   FLAGS-QUAD = -O3
	   FLAGS-HEXA = -O3

	CWD=$(shell pwd)
	PLATAFORM=$(shell hostname)
	ECHO=@echo 
	PRINTF=@printf 
	GREEN=\e[;32m
	MAG=\e[;35m
	NC=\e[0m
	BASENAME = rsmear2


#	ifeq	($(PLATAFORM),medusa)
#	NAME=$(BASENAME)_xeon
#	F90=$(COMPILER-XEON)
#	OPT = $(FLAGS-XEON)
#	WHATPL=XEON
#	endif

	ifeq	($(PLATAFORM),itanium01)
	NAME=$(BASENAME)_itanium
	F90=$(COMPILER-ITAN)
	OPT = $(FLAGS-ITAN)
	WHATPL=ITAN
	endif

	ifeq	($(PLATAFORM),quad01)
	NAME=$(BASENAME)_quad
	F90=$(COMPILER-QUAD)
	OPT = $(FLAGS-QUAD)
	WHATPL=QUAD
	endif

	ifeq	($(PLATAFORM),medusa)
	NAME=$(BASENAME)_hexa
	F90=$(COMPILER-HEXA)
	OPT = $(FLAGS-HEXA)
	WHATPL=HEXA
	endif

SRCS =	smear2.f90

OBJS =	smear2.o

$(NAME): $(OBJS)
	$(F90) $(OPT) -o $@ $(OBJS) $(LIBS)
	 rm -f $(OBJS) *.mod *~ *.o 

	 cp $(NAME) ../

	 $(PRINTF) " \n " 
	 $(PRINTF) " \t $(MAG)***********************************$(NC)\n " 
	 $(PRINTF) " \t  Compiled : $(WHATPL)\n " 
	 $(PRINTF) " \t  Output   :../$(GREEN)$(NAME) $(NC)   \n " 
	 $(PRINTF) " \t $(MAG)***********************************$(NC)\n " 
clean:
	 rm -f $(OBJS) *.mod *~ *.o 

help:
	 $(ECHO) "you are compiling :$(WHATPL)  "   
	 $(ECHO) "  make         -Compila and link "    
	 $(ECHO) "  make clean   -clean all *.o *.mod " 
.SUFFIXES: $(SUFFIXES) .f90

.f90.o:
	$(F90) $(F90FLAGS) -c $<

smear2.o: smear2.o
