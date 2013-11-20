#!/bin/bash
P=$PWD
echo ITANIUM
rsh itanium01 "cd $P; make -f Makefile_vc_itan clean; make -f  Makefile_vc_itan" 
#read -p "press any key"
echo QUAD
rsh quad01    "cd $P; make -f Makefile_vc_quad clean; make -f  Makefile_vc_quad" 
#read -p "press any key"
echo XEON
make -f Makefile_vc_xeon clean; make -f  Makefile_vc_xeon
