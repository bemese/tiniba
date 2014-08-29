#!/bin/bash
P=$PWD
rsh quad01 "cd $P; make -f Makefilequad clean; make -f Makefilequad" 
#rsh quad01 "cd $P;  make -f Makefilequad" 
