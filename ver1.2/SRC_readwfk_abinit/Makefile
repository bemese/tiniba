#
FC = ifort

OBS = DebugMod.o \
      UtilitiesMod.o \
      defs_basis.o \
      defs_datatypes.o \
      hdr_io.o \
      rhoij_utils.o \
      CommandLineArgumentsMod.o \
      AbinitReaderMod.o


readwfk: main.f90 ${OBS}
	${FC} -o readwfk main.f90 ${OBS}

#	mv readwfk ../.


DebugMod.o: DebugMod.f90
	${FC} -c DebugMod.f90

UtilitiesMod.o: UtilitiesMod.f90
	${FC} -c UtilitiesMod.f90

defs_basis.o: defs_basis.F90
	${FC} -c defs_basis.F90

defs_datatypes.o: defs_datatypes.F90
	${FC} -c defs_datatypes.F90

hdr_io.o: hdr_io.F90
	${FC} -c  defs_basis.F90 defs_datatypes.F90 hdr_io.F90

rhoij_utils.o: rhoij_utils.F90
	${FC} -c rhoij_utils.F90

CommandLineArgumentsMod.o: CommandLineArgumentsMod.f90
	${FC} -c UtilitiesMod.f90  DebugMod.f90 CommandLineArgumentsMod.f90
AbinitReaderMod.o: AbinitReaderMod.f90
	${FC} -c defs_basis.F90 defs_datatypes.F90 DebugMod.f90 UtilitiesMod.f90 CommandLineArgumentsMod.f90  AbinitReaderMod.f90

clean:
	rm -rf *.o *.mod
