#!/bin/bash
P=$PWD
rsh quad01    "cd $P; make -f Makefilequad clean; make -f Makefilequad"
rsh itanium01 "cd $P; make -f Makefile64b clean; make -f  Makefile64b" 
rsh master    "cd $P; make -f Makefile32b clean; make -f  Makefile32b"
