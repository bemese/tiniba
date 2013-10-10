#!/bin/bash
P=$PWD
rsh quad01 "cd $P; make -f Makefilequad_caleta_with_calDelta clean; make -f Makefilequad_caleta_with_calDelta" 
