#!/bin/bash
P=$PWD
# medusa&hexa
make -f Makefilehexa clean; make -f  Makefilehexa 
# quad
#ssh quad01    "cd $P; make -f Makefilequad clean; make -f Makefilequad"
# itanium
#ssh itanium01 "cd $P; make -f Makefile64b clean; make -f  Makefile64b"
# xeon (needs to be modified)
# make -f Makefile32b clean; make -f  Makefile32b 
#rsh master    "cd $P; make -f Makefile32b clean; make -f  Makefile32b"
