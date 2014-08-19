#!/bin/bash
# A quick and dirty script for compiling on the different sections of Medusa

## Medusa & hexas
make -f Makefilehexa clean; make -f  Makefilehexa 

## quads
#ssh quad01 "cd $PWD; make -f Makefilequad clean; make -f Makefilequad"

## itanium
#ssh itanium01 "cd $P; make -f Makefile64b clean; make -f  Makefile64b"

## xeon (needs to be modified)
# make -f Makefile32b clean; make -f  Makefile32b 
# rsh master    "cd $P; make -f Makefile32b clean; make -f  Makefile32b"
