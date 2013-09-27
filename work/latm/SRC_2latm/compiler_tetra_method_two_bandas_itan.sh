#!/bin/bash
P=$PWD
rsh itanium01 "cd $P; make -f Makefile_two_bands_itan clean; make -f  Makefile_two_bands_itan" 
