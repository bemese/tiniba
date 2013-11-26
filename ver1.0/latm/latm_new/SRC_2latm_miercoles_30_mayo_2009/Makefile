SRC = tetra_method.f90 globals.f90 piksort.f90 inparams.f90
OBJS = constants.o inparams.o globals.o piksort.o tetra_method.o
MODSRC = globals.f90 inparams.f90 constants.f90
#FC = pgf90
#FC = ifc
FC = lf95
#SAFE = -CB
#SAFE = --chk
#OPT = -Vaxlib -w -O3
#OPT = --ap --chk --chkglobal --li --trap
OPT = -O3
tetra_method : ${OBJS}
	${FC} ${OPT} ${SAFE} -o tetra_method $(OBJS)
	mv tetra_method ../.

constants.o : constants.f90
	${FC} ${OPT} ${SAFE} -c constants.f90

inparams.o : inparams.f90 constants.o
	${FC} ${OPT} ${SAFE} -c inparams.f90

globals.o : globals.f90 inparams.o
	${FC} ${OPT} ${SAFE} -c globals.f90

piksort.o : piksort.f90
	${FC} ${OPT} ${SAFE} -c piksort.f90

tetra_method.o : tetra_method.f90 globals.o
	${FC} ${OPT} ${SAFE} -c tetra_method.f90

clean :
	rm -f *.o

all : ${OBJS} tetra_method
